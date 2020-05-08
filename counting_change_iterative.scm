#lang sicp

(define (count-change x)
  (define (count-change-itr n-5 n-10 n-25 times)
    (cond ((>= (* n-5 5) 10) (count-change-itr (- n-5 2)
                                               (+ n-10 1)
                                               n-25
                                               (+ times 1)))
          ((>= (* n-10 10) 30) (count-change-itr (+ n-5 1 (* (- n-10 3) 2))
                                                 0
                                                 (+ n-25 1)
                                                 (+ times 1)))
          ((=  (+ (* n-5 5) (* n-10 10)) 25) (count-change-itr (+ n-5 (* (- n-10 2) 2))
                                                               0
                                                               (+ n-25 1)
                                                               (+ times 1)))
          (else times)))

  (count-change-itr (/ x 5) 0 0 1))

(count-change 30)
(count-change 60)
