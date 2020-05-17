#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (let ((n-i (n i))
          (d-i (d i)))
      (cond ((= k i) (/ n-i d-i))
            (else (/ n-i (+ d-i (cont-frac-iter (+ i 1))))))))

  (cont-frac-iter 1)
  )

(/ 1 (cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           12))

; it takes 12 iterations to get 4 decimal places of accuracy.
; This is a recursive process
