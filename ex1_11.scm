#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))
  
(define (fast-exp b n)

  (define (iter b n a)
    (cond ((= n 0) a)
          ((not (even? n)) (iter b (- n 1) (* a b)))
          (else (iter (square b) (/ n 2) a))))
  (iter b n 1))

(fast-exp 2 14)
(fast-exp 2 16)