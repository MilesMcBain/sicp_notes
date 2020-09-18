#lang sicp
(#%require sicp-pict)

(define a
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 0 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 1)
                                         (make-vect 1 0))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 0)))))

(define b
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 1 0)))))

(define c
  (segments->painter (list (make-segment (make-vect 0.5 0)
                                         (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5)
                                         (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1)
                                         (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 0)))))

;; wave painter...
;; Ummm... no? let's not code that.
;; You could roll a simplification just with straight lines,
;; or you could make the lines so small they're pixels and draw the whole thing with those.
(define d
  (segments->painter))
