#lang sicp

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (repeated fn n)
  (define (repeat-make fn-chain n)
    (cond ((= n 1) fn-chain)
          (else (repeat-make (compose fn fn-chain) (- n 1)))))
  (repeat-make fn n)
  )

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

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try-guess guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-guess next))))
  (try-guess first-guess)
  )

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (avg-damped-nth-root x n-root damps)
  (fixed-point ((repeated average-damp damps)
                (lambda (y) (/ x
                               (fast-exp y (- n-root 1)))))
               1.0))


(avg-damped-nth-root (fast-exp 5 16) 16 4)

;; root, avg-damps
;; 1 - 3 , 1
;; 4 - 7, 2
;; 10 - 15, 3
;; 16, 4
;; so it looks like (floor (log-base2 n-root)) for damps to make converge
;; trivial to write a wrapper that implements this assuming
;; those arithmetic ops exist as per question.


