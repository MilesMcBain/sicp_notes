#lang sicp

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imaginary-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op:" op))))
    dispatch)

(define complex-num (make-from-mag-ang 1 3.14))

(complex-num 'real-part)

