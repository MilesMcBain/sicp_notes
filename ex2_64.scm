#lang sicp

;; tree interface
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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
                    cdr non-left-elts)
                   right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                   (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts)))))))

;; This works by recursively splitting the input list in two.
;; The first half of the list is allocated to the left branch and recursively
;; balanced, then root element is pulled and saved, and then the remainder of the
;; tree is allocated to the right branch and recursively balanced. The combination
;; of the left, root and right is the tree. The partial-tree procedure is called
;; once for every node and two extra times for leaf nodes.
;; surely this must be O(n) running time.

