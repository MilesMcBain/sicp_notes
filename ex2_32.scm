#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))
                          rest)))))

(subsets (list 1 2 3))

;; How this works:
;; cdrs down until it gets to (3)
;; rest of s = (3) is ()
;; append () and map (3) append () is (() (3))
;; This is returned up the stack as rest of s = (2 3)
;; append (() (3)) to map append (2) over (() (3)) gives
;; (() (3) (2) (2 3))
;; This is returned up the stack as rest of s = (1 2 3)
;; append (() (3) (2) (2 3)) to map append (1) over (() (3) (2) (2 3))
;; gives: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; The answer.
;; The trick is the repeated append map-append combo which
;; accumulates every subset seen before and combines it with a new version
;; where a single new digit is combined with each previous subset.
