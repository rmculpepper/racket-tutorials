#lang racket/base
(require racket/pretty "parser.rkt")

;; A basic LL(1) grammar:

;; Expr ::= number
;;       |  lparen Expr + Expr rparen

(define arith-grammar
  (grammar
   'Expr
   (list (def 'Expr
           (list (prod 'Expr 0
                       (list (telem 'number))
                       (lambda (a) a))
                 (prod 'Expr 1
                       (list (telem 'lparen)
                             (ntelem 'Expr)
                             (telem '+)
                             (ntelem 'Expr)
                             (telem 'rparen))
                       (lambda (lp e1 pl e2 rp) (list 'add e1 e2))))))))

(define arith-table (make-ll1-table arith-grammar))

(define arith-s1 '(lparen (number . 5) + (number . 12) rparen))
(ll1-parse 'Expr arith-table arith-s1)

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
   (list (def 'S
           (list (prod 'S 0
                       (list (telem 'lparen) (ntelem 'X))
                       (lambda (lp x) (list 'S 0 x)))
                 (prod 'S 1
                       (list (ntelem 'E) (telem 'rbracket))
                       (lambda (e rb) (list 'S 1 e)))
                 (prod 'S 2
                       (list (ntelem 'F) (telem 'rparen))
                       (lambda (f rp) (list 'S 2 f)))))
         (def 'X
           (list (prod 'X 0
                       (list (ntelem 'E) (telem 'rparen))
                       (lambda (e rb) (list 'X 0 e)))
                 (prod 'X 1
                       (list (ntelem 'F) (telem 'rbracket))
                       (lambda (f rb) (list 'X 1 f)))))
         (def 'E
           (list (prod 'E 0 (list (ntelem 'A)) (lambda (a) (list 'E 0 a)))))
         (def 'F
           (list (prod 'F 0 (list (ntelem 'A)) (lambda (a) (list 'F 0 a)))))
         (def 'A
           (list (prod 'A 0 (list) (lambda () (list 'A 0))))))))

(define brackets-table (make-ll1-table brackets-grammar))

(define brackets-s1 '(lparen rparen))
(ll1-parse 'S brackets-table brackets-s1)

(define brackets-s2 '(rbracket))
(ll1-parse 'S brackets-table brackets-s2)
