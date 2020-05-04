#lang sicp

(define (count-change-combinations total)

  (define (count-combinations total max-coin times)
    (cond ((< total 0) 0)
          ((= total 0) 1)
          (else (+ times
                   (if (>= max-coin 20)
                       (count-combinations (- total 20) 20 times)
                       0)
                   (if (>= max-coin 10)
                       (count-combinations (- total 10) 10 times)
                       0)
                   (count-combinations (- total 5) 5 times)))))

  (count-combinations total 20 0))

(count-change-combinations 20)
