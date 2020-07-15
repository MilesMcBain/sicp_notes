#lang sicp

(define (deep-reverse l)
  (define (reverse-itr l rev-l)
    (cond ((null? l) rev-l)
          ((pair? (car l))
           (reverse-itr (cdr l)
                        (cons (deep-reverse (car l)) rev-l)))
          (else
           (reverse-itr (cdr l)
                        (cons (car l) rev-l)))))

  (reverse-itr l nil))


(define (fringe l)
  (define (leaf-itr l leaves)
    (cond ((null? l) leaves)
          ((pair? (car l))
           (leaf-itr (cdr l) (append (leaf-itr (car l) nil) leaves)))
          (else (leaf-itr (cdr l) (cons (car l) leaves))))
    )
    (deep-reverse (leaf-itr l nil))
  )

(define x (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))
