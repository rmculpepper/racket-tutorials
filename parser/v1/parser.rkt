#lang racket/base
(require racket/match
         racket/list
         racket/class
         racket/set
         racket/pretty)
(provide (all-defined-out))

;; ============================================================

;; A Grammar is (grammar NT (Listof Def))
(struct grammar (start defs) #:prefab)

;; A Def is (def NT (Listof Prod))
(struct def (nt prods) #:prefab)

;; A Prod is (prod NT Nat ElemSequence Action)
(struct prod (nt index elems action) #:prefab)

;; An ElemSequence is (Listof Element)
;; An Element is one of
;; - (ntelem NT)
;; - (telem Terminal)
(struct ntelem (nt) #:prefab)
(struct telem (t) #:prefab)

;; A Nonterminal (NT) is a Symbol
;; A Terminal is one of the following:
;; - symbol
;; - character
(define (ok-terminal? v) (or (symbol? v) (char? v)))

;; EOF : Terminal
(define EOF (string->unreadable-symbol "EOF"))

;; ============================================================

;; An LL1-Table is Hash[NT => LL1-Entry],
;; where LL1-Entry is Hash[Terminal => (NonemptyListof Prod)]

;; The table maps nonterminals and terminals to a nonempty list of
;; productions. If the table does not have an entry, there is a parse
;; error. If an entry contains more than one production for a given
;; nonterminal and terminal, the grammar is not LL(1) (but the table
;; can still be used by a nondeterministic parser, for example).

;; make-ll1-table : Grammar -> LL1-Table
(define (make-ll1-table g)
  (match-define (grammar start defs) g)

  ;; ----------------------------------------
  ;; Nullability

  ;; A NT is nullable if it can generate the empty sequence.

  ;; {nt,elem,elemseq}-nullable? : {NT,Elem,ElemSequence} -> Boolean
  (define (nt-nullable? sym #:h [h nt-nullable-h])
    (hash-ref h sym #t))
  (define (elem-nullable? elem #:h [h nt-nullable-h])
    (match elem [(ntelem nt) (nt-nullable? nt #:h h)] [_ #f]))
  (define (elemseq-nullable? elems #:h [h nt-nullable-h])
    (for/and ([elem (in-list elems)]) (elem-nullable? elem #:h h)))

  ;; nt-nullable-h : Hash[NT => Boolean]
  (define nt-nullable-h 
    (fixed-point
     (lambda (h)
       (for/fold ([h h]) ([d (in-list defs)])
         (hash-set h (def-nt d)
                   (for/or ([p (in-list (def-prods d))])
                     (elemseq-nullable? (prod-elems p) #:h h)))))
     (hash)))

  ;; ----------------------------------------
  ;; First sets

  ;; (nt-first nt) is the set of terminals that can appear as the
  ;; first terminal in a sequence that nt generates.

  ;; {nt,elem,elemseq}-first : {NT,Elem,ElemSequence} -> (Listof Terminal)
  (define (nt-first nt #:h [h nt-first-h])
    (hash-ref h nt null))
  (define (elem-first elem #:h [h nt-first-h])
    (match elem [(ntelem nt) (nt-first nt #:h h)] [(telem t) (list t)]))
  (define (elemseq-first elems #:h [h nt-first-h])
    (let loop ([elems elems])
      (cond [(null? elems) null]
            [else (set-union (elem-first (car elems) #:h h)
                             (if (elem-nullable? (car elems)) (loop (cdr elems)) null))])))

  ;; nt-first-h : Hash[NT => (Listof Terminal)]
  (define nt-first-h
    (fixed-point
     (lambda (h)
       (for/hash ([d (in-list defs)])
         (values (def-nt d)
                 (apply set-union (hash-ref h (def-nt d) null)
                        (for/list ([p (in-list (def-prods d))])
                          (elemseq-first (prod-elems p) #:h h))))))
     (hash)))

  ;; ----------------------------------------
  ;; Follow sets

  ;; (nt-follow nt) is the set of terminals that can appear after nt
  ;; in some sentential form.

  ;; nt-follow-h : Hash[NT => (Listof Terminal)]
  (define nt-follow-h
    (fixed-point
     (lambda (h)
       (for*/fold ([h h]) ([d (in-list defs)] [pr (in-list (def-prods d))])
         (for/fold ([h h] [follows-this (hash-ref h (def-nt d) null)] #:result h)
                   ([elem (in-list (reverse (prod-elems pr)))])
           (match elem
             [(ntelem nt)
              (values (hash-set h nt (set-union (hash-ref h nt null) follows-this))
                      (set-union (nt-first nt)
                                 (if (nt-nullable? nt) follows-this null)))]
             [(telem t) (values h (list t))]))))
     (hash start (list EOF))))

  ;; nt-follow : NT -> (Listof Terminal)
  (define (nt-follow nt)
    (hash-ref nt-follow-h nt null))

  ;; ----------------------------------------
  ;; LL1 Table

  ;; def->table-entry : NTDef -> Hash[Terminal => (NonemptyListof Prod)]
  (define (def->table-entry d)
    (match-define (def nt rhss) d)
    (for/fold ([h (hash)]) ([p (in-list rhss)])
      (match-define (prod nt index elems action) p)
      (define ts (set-union (elemseq-first elems)
                            (if (elemseq-nullable? elems) (nt-follow nt) null)))
      (for/fold ([h h]) ([t (in-list ts)])
        (hash-cons h t p))))

  (for/fold ([h (hash)]) ([d (in-list defs)])
    (hash-set h (def-nt d) (def->table-entry d))))

;; table-conflicts : LL1-Table -> (Listof ???)
(define (table-check-conflicts table)
  (for*/list ([(nt entry) (in-hash table)]
              [(t prods) (in-hash entry)]
              #:when (> (length prods) 1))
    (list nt t (map prod-index prods))))

;; fixed-point : (X -> X) X -> X
(define (fixed-point refine init-val)
  (let loop ([current-val init-val])
    (define new-val (refine current-val))
    (if (equal? new-val current-val) new-val (loop new-val))))

;; hash-cons : Hash[K => (Listof V)] K V -> Hash[K => (Listof V)]
(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))

;; ============================================================

;; A TokenValue is one of
;; - (cons Terminal Any)   -- token with payload
;; - Terminal              -- token with self as payload; eg, #\x equiv to (cons #\x #\x)

;; token : Terminal [Any] -> TokenValue
(define (token t [v t]) (if (eqv? t v) t (cons t v)))

;; token-t : TokenValue -> Terminal
(define (token-t tok)
  (match tok [(cons t _) t] [(? ok-terminal? t) t]))

;; token-v : TokenValue -> Any
(define (token-v tok)
  (match tok [(cons _ v) v] [(? ok-terminal? t) #f]))

;; EOF-token : TokenValue
(define EOF-token EOF)

;; ============================================================

;; A TokenStream is (Listof Token), but we treat it as being followed by
;; an infinite stream of EOF tokens.

;; tokens-first : TokenStream -> Token
(define (tokens-first toks) (if (pair? toks) (car toks) EOF-token))

;; tokens-rest : TokenStream -> TokenStream
(define (tokens-rest toks) (if (pair? toks) (cdr toks) null))

;; ll1-parse* : NT LL1-Table TokenStream -> (values Any TokenStream)
(define (ll1-parse* start table toks)

  ;; parse-nt : NT TokenStream -> (values Any TokenStream)
  (define (parse-nt nt toks)
    (cond [(hash-ref (hash-ref table nt) (token-t (tokens-first toks)) #f)
           => (lambda (prs) (parse-prod (car prs) toks))]
          [else (error 'll1-parse "parse error\n  parsing NT: ~e\n  next token: ~e"
                       nt (tokens-first toks))]))

  ;; parse-elem : Elem TokenStream -> (values Any TokenStream)
  (define (parse-elem e toks)
    (match e
      [(ntelem nt)
       (parse-nt nt toks)]
      [(telem t)
       (define next-tok (tokens-first toks))
       (if (equal? t (token-t next-tok))
           (values (token-v next-tok) (tokens-rest toks))
           (error 'll1-parse "parse error\n  expected: ~e\n  next token: ~e" t next-tok))]))

  ;; parse-prod : Prod TokenStream -> (values Any TokenStream)
  (define (parse-prod p toks)
    (match-define (prod nt index elems action) p)
    (define-values (r-results toks*)
      (for/fold ([r-results null] [toks toks]) ([e (in-list elems)])
        (define-values (result toks*) (parse-elem e toks))
        (values (cons result r-results) toks*)))
    (values (apply action (reverse r-results)) toks*))

  (parse-nt start toks))

;; ll1-parse* : NT LL1-Table TokenStream -> (values Any TokenStream)
(define (ll1-parse start table toks)
  (define-values (result toks*) (ll1-parse* start table toks))
  (unless (null? toks*) (error 'll1-parse "tokens left over after parsing"))
  result)
