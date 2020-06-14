#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))



(add-1 zero)
;; yikes
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; but invoking inner lambda of f just returns idenity
(lambda (f) (lambda (x) (f (((lambda (x) x)) x))))
;; and then identity of x is x
(lambda (f) (lambda (x) (f x)))
;; so then this is one
(define one (lambda (f) (lambda (x) (f x))))
;; now add-1 to 1
(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; so again we ignore f and return inner lambda
(lambda (f) (lambda (x) (f (((lambda (x) (f x))) x))))
;; and then sub in x
(lambda (f) (lambda (x) (f (f x))))
;; so this is two and the pattern is wrapping the body of inner
;; lambda in successive f's
(define (add a b)
  (lambda (x) ((a x) ((b x) x)))
;; try 1, 2


(lambda (x) ((lambda (x) (f x))
             (f (f x))))

(lambda (x) (f (f (f x))))

;; okay so we're missing a lambda f
;; just wrap:
(define (add a b)
  (lambda (f) (lambda (x) ((a x) ((b x) x)))))

;; correct according to:
;; https://codology.net/post/sicp-solution-exercise-2-6/
