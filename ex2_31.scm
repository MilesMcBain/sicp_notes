#lang sicp

(define (square x) (* x x))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(define (tree-map tree-procedure l)

  (define (tree-map-node l)
    (tree-map tree-procedure l))

  (if (pair? l)
      (map tree-map-node l)
      (tree-procedure l)))

(define (square-tree l)
  (tree-map square l))

(square-tree x)

(define (tree-map2 tree-procedure l)
  (map (lambda (node)
         (if (pair? node)
             (tree-map2 tree-procedure node)
             (tree-procedure node)))
       l))

(define (square-tree2 l)
  (tree-map2 square l))

;; I see now why the lambda was used.
;; It allows reference to tree-procedure from the enclosing function.
;; map can't work like (map tree-map tree-procedure l)
;; Since each arg other than mapping function needs to be a list of
;; the same length.
