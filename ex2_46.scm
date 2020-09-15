#lang sicp

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

;; testing
(make-vect 0 0)

(xcor-vect (make-vect 1 0))
(ycor-vect (make-vect 0 1))

(add-vect (make-vect 0 1) (make-vect 1 0))
(sub-vect (make-vect 0 1) (make-vect 1 0))
(scale-vect (make-vect 1 1) 5)
