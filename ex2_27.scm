#lang sicp

(define (deep-reverse l)
  (define (reverse-itr l rev-l)
    (cond ((null? l) rev-l)
          ((pair? (car l)) (reverse-itr (cdr l)
                                  (cons (deep-reverse (car l)) rev-l)))
          (else (reverse-itr (cdr l)
                             (cons (car l) rev-l)))))

  (reverse-itr l nil))

(define x (list (list 1 2) (list 3 4)))

(cdr x)
(car x)

(deep-reverse (car x))
(deep-reverse x)
