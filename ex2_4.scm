#lang sicp

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 pair)
  (pair (lambda (p q) p)))

(define (cdr2 pair)
  (pair (lambda (p q) q)))

(define tst (cons2 1 2))

(car2 tst)

(cdr2 tst)
