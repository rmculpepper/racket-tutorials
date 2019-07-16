#lang racket/base
(require racket/pretty "sexpr-frontend.rkt")

;; A basic LL(1) grammar:

;; Expr ::= number
;;       |  lparen Expr + Expr rparen

(define arith-parser
  (ll1-parser
   'Expr
   `([Expr [(number) ,(lambda (n) n)]
           [(lparen Expr + Expr rparen)
            ,(lambda (lp e1 pl e2 rp) (list 'add e1 e2))]])))

(define arith-s1 '(lparen (number . 5) + (number . 12) rparen))
(arith-parser arith-s1)

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

(define brackets-parser
  (ll1-parser
   'S
   `([S [(lparen X) ,(lambda (lp x) (list 'S 0 x))]
        [(E rbracket) ,(lambda (e rb) (list 'S 1 e))]
        [(F rparen) ,(lambda (f rp) (list 'S 2 f))]]
     [X [(E rparen) ,(lambda (e rp) (list 'X 0 e))]
        [(F rbracket) ,(lambda (f rb) (list 'X 1 f))]]
     [E [(A) ,(lambda (a) (list 'E 0 a))]]
     [F [(A) ,(lambda (a) (list 'F 0 a))]]
     [A [() ,(lambda () (list 'A 0))]])))

(define brackets-s1 '(lparen rparen))
(brackets-parser brackets-s1)

(define brackets-s2 '(rbracket))
(brackets-parser brackets-s2)
