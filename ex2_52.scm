#lang sicp
(#%require sicp-pict)


(define wave
  (segments->painter (list (make-segment (make-vect 0.1 0.0)
                                         (make-vect 0.2 0.3))
                           (make-segment (make-vect 0.2 0.3)
                                         (make-vect 0.3 0.5))
                           (make-segment (make-vect 0.3 0.5)
                                         (make-vect 0.3 0.6))
                           (make-segment (make-vect 0.3 0.6)
                                         (make-vect 0.1 0.7))
                           (make-segment (make-vect 0.1 0.7)
                                         (make-vect 0.0 0.9))
                           (make-segment (make-vect 0.0 0.9)
                                         (make-vect 0.1 0.8))
                           (make-segment (make-vect 0.1 0.8)
                                         (make-vect 0.3 0.7))
                           (make-segment (make-vect 0.3 0.7)
                                         (make-vect 0.5 0.7))
                           (make-segment (make-vect 0.5 0.7)
                                         (make-vect 0.4 0.8))
                           (make-segment (make-vect 0.4 0.8)
                                         (make-vect 0.4 0.9))
                           (make-segment (make-vect 0.4 0.9)
                                         (make-vect 0.5 1.0))
                           (make-segment (make-vect 0.5 1.0)
                                         (make-vect 0.6 0.9))
                           (make-segment (make-vect 0.6 0.9)
                                         (make-vect 0.6 0.8))
                           (make-segment (make-vect 0.6 0.8)
                                         (make-vect 0.5 0.7))
                           (make-segment (make-vect 0.5 0.7)
                                         (make-vect 0.8 0.7))
                           (make-segment (make-vect 0.8 0.7)
                                         (make-vect 1.0 0.5))
                           (make-segment (make-vect 1.0 0.5)
                                         (make-vect 0.8 0.6))
                           (make-segment (make-vect 0.8 0.6)
                                         (make-vect 0.7 0.6))
                           (make-segment (make-vect 0.7 0.6)
                                         (make-vect 0.7 0.5))
                           (make-segment (make-vect 0.7 0.5)
                                         (make-vect 0.8 0.3))
                           (make-segment (make-vect 0.8 0.3)
                                         (make-vect 0.9 0.0))
                           (make-segment (make-vect 0.7 0.0)
                                         (make-vect 0.6 0.3))
                           (make-segment (make-vect 0.6 0.3)
                                         (make-vect 0.5 0.4))
                           (make-segment (make-vect 0.5 0.4)
                                         (make-vect 0.4 0.3))
                           (make-segment (make-vect 0.4 0.3)
                                         (make-vect 0.3 0.0)))))


(paint wave)

(define (split first-partition second-partition)
  (define (do-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (do-split painter (- n 1))))
          (first-partition painter
                           (second-partition smaller smaller)))))

  (lambda (painter n)
    (do-split painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split wave 5))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let((combine4 (square-of-four flip-horiz identity
                                 rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit wave 6) #:width 1000 #:height 1000)
