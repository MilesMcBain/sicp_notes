#lang sicp


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; (define (adjoin-set x set)
;;   (if (element-of-set? x set)
;;       set
;;       (cons x set)))

(define (adjoin-set x set)
  (cond ((null? x) set)
        ((equal? set '()) (cons x set))
        ((equal? x (car set)) set)
        ((< x (car set))
         (cons x set))
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 5 (list 1 2 3))

(adjoin-set 0 (list 1 2 3))

(adjoin-set 3 (list 1 2 3))

(adjoin-set 3 (list 1 2 4))
