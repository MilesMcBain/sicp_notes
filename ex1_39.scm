#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-iter result i)
    (let ((d-i (d i))
          (n-i (n i)))
      (cond ((= k i) (cont-frac-iter (/ n-i d-i)
                                     (- i 1)))
            ((= i 0) result)
            (else (cont-frac-iter (/ n-i (- d-i result)) ;; flipped this to a minus!
                                  (- i 1))))))
  (cont-frac-iter 0 k)
  )

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (* x x)))
             (lambda (i) (- (* 2 i) 1))
             k
             )
  )

(tan-cf 1.0 25)
