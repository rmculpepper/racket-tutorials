#lang racket/base
(require racket/pretty "parser.rkt")

;;## BEGIN CODE test-arith
;; A basic LL(1) grammar:

;; Expr ::= number
;;       |  lparen Expr + Expr rparen

(define arith-grammar
  (grammar
   'Expr
   (list (definition 'Expr
           (list (production (list (telem 'number))
                             (lambda (a) a))
                 (production (list (telem 'lparen)
                                   (ntelem 'Expr)
                                   (telem '+)
                                   (ntelem 'Expr)
                                   (telem 'rparen))
                             (lambda (lp e1 pl e2 rp)
                               (list 'add e1 e2))))))))

(define arith-table (make-ll1-table arith-grammar))

(define arith-s1 '(lparen (number . 5) + (number . 12) rparen))
(ll1-parse 'Expr arith-table arith-s1)
;;## END CODE test-arith

;; ----

;; A grammar that is LL(1) but not LALR(1):
;; Reference: https://stackoverflow.com/questions/6487588

;; S ::= ( X
;;    |  E ]
;;    |  F )
;; X ::= E )
;;    |  F ]
;; E ::= A
;; F ::= A
;; A ::= ε

(define brackets-grammar
  (grammar
   'S
   (list (definition 'S
           (list (production (list (telem 'lparen) (ntelem 'X))
                             (lambda (lp x) (list 'S 0 x)))
                 (production (list (ntelem 'E) (telem 'rbracket))
                             (lambda (e rb) (list 'S 1 e)))
                 (production (list (ntelem 'F) (telem 'rparen))
                             (lambda (f rp) (list 'S 2 f)))))
         (definition 'X
           (list (production (list (ntelem 'E) (telem 'rparen))
                             (lambda (e rb) (list 'X 0 e)))
                 (production (list (ntelem 'F) (telem 'rbracket))
                             (lambda (f rb) (list 'X 1 f)))))
         (definition 'E
           (list (production (list (ntelem 'A)) (lambda (a) (list 'E 0 a)))))
         (definition 'F
           (list (production (list (ntelem 'A)) (lambda (a) (list 'F 0 a)))))
         (definition 'A
           (list (production (list) (lambda () (list 'A 0))))))))

(define brackets-table (make-ll1-table brackets-grammar))

(define brackets-s1 '(lparen rparen))
(ll1-parse 'S brackets-table brackets-s1)

(define brackets-s2 '(rbracket))
(ll1-parse 'S brackets-table brackets-s2)
