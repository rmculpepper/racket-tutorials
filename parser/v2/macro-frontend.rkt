#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/list
         (prefix-in sexpr: "sexpr-frontend.rkt"))
(provide ll1-parser)

(begin-for-syntax
  (define (ok-terminal? v) (or (symbol? v) (char? v)))
  (define-syntax-class ntdef
    (pattern [nt:id p:prod ...]
             #:with expr #'(list (quote nt) p.expr ...)))
  (define-syntax-class prod
    (pattern [es:elemseq action:expr]
             #:with expr #'(list (quote es) action)))
  (define-syntax-class elemseq
    (pattern (e:elem ...)))
  (define-syntax-class elem
    (pattern t/nt:id)
    (pattern t #:when (ok-terminal? (syntax-e #'t))))
  (void))

(define-syntax ll1-parser
  (syntax-parser
    [(_ start:id (d:ntdef ...))
     #'(sexpr:ll1-parser (quote start) (list d.expr ...))]))
