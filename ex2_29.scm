#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (terminal? branch)
  (not (pair? (structure branch))))

(define (structure branch)
  (car (cdr branch)))

(define (branch-length branch)
  (car branch))

;; b
(define (total-weight mobile)

  (define (total-weight-recur branch)
    (cond ((terminal? branch) (structure branch))
          (else (+ (total-weight-recur
                      (left-branch (structure branch)))
                     (total-weight-recur
                      (right-branch (structure branch)))))))
  (total-weight-recur (make-branch 0 mobile)))

(define x (make-mobile (make-branch 1 4)
                       (make-branch 3 (make-mobile (make-branch 1 2)
                                                   (make-branch 1 2)))))

(total-weight x)

;; c
;; For this I introduce the concept of balanced weight.
;; A structure has a balanced weight if it is balanced.
;; If it is unbalanced its balanced weight is nil.
;; A mobile can only be balanced if all sub-modules are balanced.
(define (balanced-weight mobile)
  (define (balanced-weight-recur branch)
    (cond ((terminal? branch) (* (branch-length branch)
                                 (structure branch)))
          (else
           (let ((l-branch (balanced-weight-recur (left-branch (structure branch))))
                 (r-branch (balanced-weight-recur (right-branch (structure branch)))))
             (cond ((or (null? l-branch) (null? r-branch)) nil)
                   ((not (= l-branch r-branch)) nil)
                   (else (* (+ l-branch r-branch) (branch-length branch))))))))
  (balanced-weight-recur (make-branch 1 mobile)))
;; little bit of a fudge here. We need to set the branch length of the fake top-level branch to 1 so that
;; it doesn't scale up or down the weight.

(define y (make-mobile (make-branch 1 4)
                       (make-branch 1 (make-mobile (make-branch 1 2)
                                                   (make-branch 1 2)))))

(define z (make-mobile (make-branch 1 4)
                       (make-branch 1 5)))

(map balanced-weight (list x y z))

(define (balanced? mobile)
  (not (null? (balanced-weight mobile))))

(map balanced? (list x y z))

;; d
;; If we changed the representation of mobile and branch to use cons instead of list
;; we'd update:
;; right-branch
;; structure
;; branch-length
;; These are all very simple procedures. The algorithms in c and d
;; are successfully implemented in terms of these primitives without reference to the underlying data
;; structure.
