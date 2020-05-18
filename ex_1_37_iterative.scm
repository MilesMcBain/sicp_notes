; The approach to take is to start from the last term and build on top of it

#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-iter result i)
    (let ((d-i (d i))
          (n-i (n i)))
      (cond ((= k i) (cont-frac-iter (/ n-i d-i)
                                     (- i 1)))
            ((= i 1) result)
            (else (cont-frac-iter (/ n-i (+ d-i result))
                                  (- i 1))))))
  (cont-frac-iter 0 k)
  )

(/ 1 (cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           13
           ))

