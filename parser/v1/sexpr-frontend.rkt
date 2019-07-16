#lang racket/base
(require racket/match
         racket/list
         "parser.rkt")
(provide ll1-parser)

(define (ntdef-sexpr->nt ntdef)
  (match ntdef
    [(cons (? symbol? nt) _) nt]
    [_ (error 'll1-parser "bad NT definition: ~e" ntdef)]))

(define (ntdef-sexpr->ntdef ntdef nt?)
  (match ntdef
    [(list (? symbol? nt) prods ...)
     (def nt (for/list ([prod (in-list prods)] [index (in-naturals)])
               (prod-sexpr->prod prod nt? nt index)))]
    [_ (error 'll1-parser "bad NT definition: ~e" ntdef)]))

(define (prod-sexpr->prod pr nt? nt index)
  (match pr
    [(list elemseq (? procedure? action))
     (define elems (elemseq-sexpr->elems elemseq nt?))
     (unless (procedure-arity-includes? action (length elems))
       (error 'll1-parser "bad action routine arity\n  procedure: ~e\n  expected: ~s"
              action (length elems)))
     (prod nt index elems action)]
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

(define (ll1-parser start ntdefs)
  (define nts (map ntdef-sexpr->nt ntdefs))
  (define (nt? s) (memq s nts))
  (let ([dup (check-duplicates nts)])
    (when dup (error 'll1-parser "duplicate nonterminal definition: ~e" dup)))
  (unless (nt? start)
    (error 'll1-parser "start symbol not defined as nonterminal: ~e" start))
  (define g (grammar start (for/list ([ntdef (in-list ntdefs)])
                             (ntdef-sexpr->ntdef ntdef nt?))))
  (define table (make-ll1-table g))
  (define (parser toks) (ll1-parse start table toks))
  parser)
