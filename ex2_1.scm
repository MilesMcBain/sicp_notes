#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (numer-sign (cond ((and (< n 0) (< d 0)) 1)
                          ((and (>= n 0) (>= d 0)) 1)
                          (else -1))))
    (cons (* (abs (/ n g)) numer-sign)
          (abs (/ d g) ))))

(define (numer r) (car r))

(define (denom r) (cdr r))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom y) (denom x) )))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom y) (denom x) )))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r))
  (newline))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-third one-third))

(print-rat (make-rat -1 -2))

(print-rat (make-rat -1 2))
