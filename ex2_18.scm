#lang sicp

(define (ugly-reverse l)
  (cond ((not (list? (car l))) (ugly-reverse (cons (cons (car l) nil) (cdr l))))
        ((null? (cdr l)) (car l))
        (else (ugly-reverse (cons (cons (car (cdr l)) (car l)) (cdr (cdr l)))))))
;; works but is ugly and complicated

(define (reverse l)
  (define (reverse-itr l rev-l)
    (if (null? l)
        rev-l
        (reverse-itr (cdr l) (cons (car l) rev-l))))
  (reverse-itr (cdr l) (cons (car l) nil)))


(ugly-reverse (list 1 4 9 16 29))
(reverse (list 1 4 9 16 29))


