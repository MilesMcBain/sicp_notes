#lang sicp
(#%require sicp-pict)


;; implementation 1
(define (below painter-upper painter-lower)
  (let ((paint-up (transform-painter painter-upper
                                     (make-vect 0.0 0.5)
                                     (make-vect 1.0 0.5)
                                     (make-vect 0.0 1.0)))
        (paint-down (transform-painter painter-lower
                                       (make-vect 0.0 0.0)
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.0 0.5))))
    (lambda (frame)
      (paint-up frame)
      (paint-down frame))))

(paint (below einstein einstein))


;; implementation 2

(define (rotate-90-anti painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate-90-clock painter)
  (transform-painter painter
                      (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0)))

(define (below2 painter1 painter2)
  (rotate-90-clock (beside (rotate-90-anti painter1)
                           (rotate-90-anti painter2))))

(paint (below2 einstein einstein))
