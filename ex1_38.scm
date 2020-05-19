#lang sicp

;;iterative version of the cont-frac
(define (cont-frac n d k)
  (define (cont-frac-iter result i)
    (let ((d-i (d i))
          (n-i (n i)))
      (cond ((= i 0) result)
            ((= k i) (cont-frac-iter (/ n-i d-i)
                                     (- i 1)))
            (else (cont-frac-iter (/ n-i (+ d-i result))
                                  (- i 1))))))
  (cont-frac-iter 0 k))

;; the hard bit is just writing some kind of function to generate the sequence:
;; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
;; if i + 1 %% 3 == 0 then (i + 1)/3 * 2
;; else 1

(define (e-seq i)
  (cond ((= (remainder (+ i 1) 3) 0) (* (/ (+ i 1) 3) 2))
        (else 1)))

(cont-frac (lambda (x) 1.0)
           e-seq
           12)


