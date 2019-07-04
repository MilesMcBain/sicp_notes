#lang sicp

(define (even? x)
  (= (remainder x 2) 0))

(define (halve x)
  (/ x 2))

(define (double x)
  (* x 2))

(define (fast-mult a b)
  (cond
    ((= b 0) 0)
    ((even? b) (double (fast-mult a (halve b))))
    (else (+ a (fast-mult a (- b 1))))))

(fast-mult 4 2)

(fast-mult 4 4)

(fast-mult 10 1)

(fast-mult 9 3)