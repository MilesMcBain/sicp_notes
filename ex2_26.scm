#lang sicp

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
;; (1 2 3 4 5 6)

(cons x y)
;; ((1 2 3) . (4 5 6))
;; but no!
;; it's ((1 2 3) 4 5 6)
;; makes sense of course. I consed something to the front of a list.
;; It just happened to be a list.

(list x y)
;; ((1 2 3) (4 5 6))
