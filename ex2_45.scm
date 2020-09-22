#lang sicp
(#%require sicp-pict)

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

(paint (right-split einstein 2))

(paint (up-split einstein 2))
