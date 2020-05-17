#lang sicp

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001)
    )
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try 1.0)
  )

(fixed-point cos 1.0)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)
