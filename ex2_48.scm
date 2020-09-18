#lang sicp
(#%require sicp-pict)

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))


(define (make-segment start-vec end-vec)
  (list start-vec end-vec))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

;; testing
(make-segment (make-vect 0 0) (make-vect 1 1))

(start-segment (make-segment (make-vect 0 0) (make-vect 1 1)))

