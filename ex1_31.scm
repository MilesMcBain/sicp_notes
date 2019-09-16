#lang racket

(define (even? x)
  (= (remainder x 2) 0))

; iterative version
(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a)
                      (* result (term a))))
    )

  (product-iter a 1))

; test with factorial
(define (fac-term x) x)

(define (fac-next x) (+ x 1))

(= (product fac-term 1 fac-next 5) 120)

; use to calculate pi/4
(define (wallis-term x)
  (if (even? x)
      (/ (+ x 2)
         (+ x 1))
      (/ (+ x 1)
         (+ x 2))))

(define (wallis-next x)
  (+ x 1)
  )

; convergence of Wallis' result is pretty slow
; so slow I thought I had a bug.
(- (product wallis-term 1 wallis-next 1000)
   (/ pi 4))

; recursive version
(define (product-recursive term a next b)
  (if (<= a b)
      (* (term a) (product-recursive term (next a) next b))
      1
      )
  )

; test recursive version

(= (product-recursive fac-term 1 fac-next 5) 120)
