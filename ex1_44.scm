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

(define dx 0.00001)

(define (smooth fn)
  (lambda (x) (/ (+ (fn (+ x dx))
                    (fn x)
                    (fn (- x dx))) 3.0)))

(define (n-fold-smooth fn folds)
  ((repeated smooth folds) fn))

(define foo-fun (n-fold-smooth inc 16))

(foo-fun 2.0)
