
#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (repeated fn n)
  (define (repeat-make fn-chain n)
    (cond ((= n 1) fn-chain)
          (else (repeat-make (compose fn fn-chain) (- n 1)))))
  (repeat-make fn n)
  )

((repeated square 3) 5)
