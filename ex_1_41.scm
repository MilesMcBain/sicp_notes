#lang sicp

(define (double f)
  (lambda (x) (f (f x)))
  )

(((double (double double)) inc) 5)

;; create a procedure that applies arg-procedure 4 times
;; apply that procedure to itself, creating a procedure that
;; applies arg-procedure 4 x 4 = 16 times
;; output is 5 + 15 = 21
