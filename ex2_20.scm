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


;; After seeing exercise 21/22

(define (same-par-recur x . d)
  (if (null? d)
      nil
      (if (same-truthy (even? x) (even? (car d)))
          (cons (car d) (same-par-recur (cons x (cdr d))))
          (same-par-recur (cons x (cdr d))))))

(same-par-recur 1 3 5 7)

(same-par-recur 2 3 4 5 6 7)

;; OH duh but this doesn't work because I can't spread the args I'm passing a
;; single list.
