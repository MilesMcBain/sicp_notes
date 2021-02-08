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

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((equal? (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))))

(union-set
 (list 2 4 6 8 10 12)
 (list 2 4 8 16 32 64))
