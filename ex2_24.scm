#lang sicp

(list 1 (list 2 (list 3 4)))

;; I think this should be (1 2 3 4)
;; Okay yes but no nil.
;; interesting how different it is from:

(cons 1 (cons 2 (cons 3 4)))
;; So I guess (list) is adding a nil a the end of each list?

(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil))
;; Yup. See handwritten notes for box diagram.
