#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-coord p)
  (car p))

(define (y-coord p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-coord p))
  (display ", ")
  (display (y-coord p))
  (display ")")
  (newline))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (+ (/ (- (x-coord (end-segment s))
                       (x-coord (start-segment s)))
                    2)
                 (x-coord (start-segment s)))
              (+ (/ (- (y-coord (end-segment s))
                       (y-coord (start-segment s)))
                    2)
                 (y-coord (start-segment s)))))

(define my-seg (make-segment (make-point 5 5)
                             (make-point 0 0)))

(print-point (midpoint-segment my-seg))
