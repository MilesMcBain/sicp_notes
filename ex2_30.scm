#lang sicp

(define (square x) (* x x))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(define (square-list l)
  (cond ((null? l) nil)
        ((not (pair? l)) (square l))
        (else (cons (square-list (car l))
                    (square-list (cdr l))))))

(square-list x)

(define (square-list-map l)
  (if (pair? l)
      (map square-list-map l)
      (square l)))

(square-list-map x)

(define (square-list-map2 l)
  (map (lambda (l-node)
         (if (pair? l-node)
             (square-list-map2 l-node)
             (square l-node))) l))

(square-list-map2 x)
