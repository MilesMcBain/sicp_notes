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

;; representation 1: height, width, centre point
(define (make-rect h w centre)
  (cons (cons h w) centre))

(define (height-rect r)
  (car (car r)))

(define (width-rect r)
  (cdr (car r)))

(define (area-rect r)
  (* (height-rect r)
     (width-rect r)))

(define (perimeter r)
  (* (+ (height-rect r)
        (width-rect r))
     2))

;; representation 2: top-left, bottom-right

(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

(define (height-rect r)
  (abs (- (y-coord top-left)
          (y-coord bottom-right))))

(define (width-rect r)
  (abs (- (x-coord bottom-right)
          (x-coord top-left))))
