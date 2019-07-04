#lang sicp

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mult b n)

  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double b) (halve n)))
          (else (iter (+ a b) b (- n 1)))))

  (iter 0 b n))

(fast-mult 4 4)
(fast-mult 1 16)
(fast-mult 3 5)
(fast-mult 0 5)
  