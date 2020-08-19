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

;; This came from enumerate-tree 159
(define (fringe2 l)
  (cond ((null? l) nil)
        ((not (pair? l)) (list l))
        (else (append (fringe2 (car l))
                      (fringe2 (cdr l)))))
  )

;; it's much clearer than what I had.
;; The trick is to make full use of append which lets
;; you join lists in natural order.
;; Using cons onto leaves means you can only push onto the front
;; in reverse order which is annoying.
;; It looks like I had the idea for append in the pair? case,
;; But I didn't thing of promoting a number to a list in order
;; to use it with append.
;; My approach is also trying to be iterative, whereas this one
;; creates a tree of function calls to resolve.
(fringe2 x)
