#lang sicp


(define (adjoin-set x set)
  (cons x set))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 2 3 4))

(define (intersection-set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Under this duplicate regime, adjoin and union become very cheap, just simple
;; append operations. Intersection becomes quite inefficient since we may call
;; element of set for the same arg1 a few times traversing all of set2 each time.
;; Memoisation could help a little.
;; In applications that involve large sets, with mainly addition or union
;; operations the duplicate representation may be preferable.
;; perhaps if you have prior information that the overlap of these large sets is likely to be minimal?
;; Sets of real numbers? E.g. lon/lats?

