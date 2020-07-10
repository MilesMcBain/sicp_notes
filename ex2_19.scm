#lang sicp

(define (count-change amount)
  (cc amount 5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define us-coins (list 1 5 10 25 50))

(cc 100 us-coins)

(cc 100 uk-coins)

;; Does the order of the coins affect the number of combinations?
;; By experimentation it does not
;; This makes sense as the algo does not care about size. At each step is creates
;; 'threads' for the number of combinations involving current coin and the the number that do not - given all prior coins.
;; So if you use a 1 then 10 it is the same as a 10 then a 1.
