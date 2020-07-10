#lang sicp

(define (reverse l)
  (define (reverse-itr l rev-l)
    (if (null? l)
        rev-l
        (reverse-itr (cdr l) (cons (car l) rev-l))))
  (reverse-itr (cdr l) (cons (car l) nil)))

(define (same-truthy a b)
  (or (and a b)
      (and (not a) (not b))))

(define (same-parity x . d)
  (let ((x-even (even? x)))

    (define (par-itr parity l)
        (cond ((null? l) parity)
              ((same-truthy (even? (car l)) x-even)
               (par-itr (cons (car l) parity) (cdr l)))
              (else (par-itr parity (cdr l)))))

    (reverse (par-itr '() d)) ))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
