#lang sicp
;; Give O(n) implementations of union-set and intersection-set

;; Use ordered intersection-set from p209 and union-set from ex2.62. Each of these is O(n)

;; Update these such that the first step is to copy both trees to lists O(n) + O(n)
;; The second step is to do the intersection or union O(n)
;; The third step is to turn result into a balanced tree O(n)
;; A series of 4 O(n) operations is still O(n).

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                   right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                   (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))



(define (do-list-op-on-trees fn tree1 tree2)
  (let ((list1 (tree->list tree1))
        (list2 (tree->list tree2)))
    (list->tree (fn list1 list2))))

(define (union-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else
         (let ((x1 (car list1))
               (x2 (car list2)))
           (cond ((= x1 x2) (cons x1 (union-list (cdr list1) (cdr list2))))
                 ((> x1 x2) (cons x2 (union-list list1 (cdr list2))))
                 ((< x1 x2) (cons x1 (union-list (cdr list1) list2))))))))

(define (intersection-list list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (let ((x1 (car list1))
            (x2 (car list2)))
        (cond ((= x1 x2) (cons x1 (intersection-list (cdr list1) (cdr list2))))
              ((> x1 x2) (intersection-list list1 (cdr list2)))
              ((< x1 x2) (intersection-list (cdr list1) list2))))))

(define (intersection-set tree1 tree2)
  (do-list-op-on-trees intersection-list tree1 tree2))

(define (union-set tree1 tree2)
  (do-list-op-on-trees union-list tree1 tree2))

(define list-a (list 2 4 6 8 10 12))
(define list-b (list 2 4 8 16 32 64))
(define tree-a (list->tree list-a))
(define tree-b (list->tree list-b))

(union-list list-a list-b)
(intersection-list list-a list-b)
(union-list list-a '())
(intersection-list list-a '())

(union-set tree-a tree-b)
(intersection-set tree-a tree-b)
