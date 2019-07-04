#lang sicp

(define (divides? n d)
  (= (remainder n d) 0))

(define (square x)
  (* x x))

(define (smallest-divisor n)

  (define (divisor-search n divisor)
    (cond ((> (square divisor) n) n)
          ((divides? n divisor) divisor)
          (else (divisor-search n (+ divisor 1)))))

  (divisor-search n 2))

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)