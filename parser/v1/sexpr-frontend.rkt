#lang racket/base
(require racket/match
         racket/list
         "parser.rkt")
(provide ll1-parser)

(define (definition-sexpr->nt def)
  (match def
    [(cons (? symbol? nt) _) nt]
    [_ (error 'll1-parser "bad nonterminal definition: ~e" def)]))

(define (definition-sexpr->definition def nt?)
  (match def
    [(list (? symbol? nt) prods ...)
     (definition nt (for/list ([prod (in-list prods)])
                      (production-sexpr->production prod nt?)))]
    [_ (error 'll1-parser "bad NT definition: ~e" def)]))

(define (production-sexpr->production prod nt?)
  (match prod
    [(list elemseq (? procedure? action))
     (define elems (elemseq-sexpr->elems elemseq nt?))
     (unless (procedure-arity-includes? action (length elems))
       (error 'll1-parser "bad action routine arity\n  procedure: ~e\n  expected: ~s"
              action (length elems)))
     (production elems action)]
    [_ (error 'll1-parser "bad production: ~e" prod)]))

(define (elemseq-sexpr->elems elemseq nt?)
  (match elemseq
    [(list elems ...)
     (for/list ([elem (in-list elems)])
       (elem-sexpr->elem elem nt?))]
    [_ (error 'll1-parser "bad element sequence: ~e" elemseq)]))

(define (elem-sexpr->elem elem nt?)
  (match elem
    [(? nt? nt) (ntelem nt)]
    [(? ok-terminal? t) (telem t)]
    [_ (error 'll1-parser "bad element: ~e" elem)]))

(define (ll1-parser start defs)
  (define nts (map definition-sexpr->nt defs))
  (define (nt? s) (memq s nts))
  (let ([dup (check-duplicates nts)])
    (when dup (error 'll1-parser "duplicate nonterminal definition: ~e" dup)))
  (unless (nt? start)
    (error 'll1-parser "start symbol not defined as nonterminal: ~e" start))
  (define g (grammar start (for/list ([def (in-list defs)])
                             (definition-sexpr->definition def nt?))))
  (define table (make-ll1-table g))
  (define (parser toks) (ll1-parse start table toks))
  parser)
