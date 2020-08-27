#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (fold-itr result rest)
    (if (null? rest)
        result
        (fold-itr (op result (car rest))
                  (cdr rest))))
  (fold-itr initial sequence))

;; guess the output

(fold-right / 1 (list 1 2 3))
;; 3, 2/3, 1/(2/3) - 1.5
(fold-left / 1 (list 1 2 3))
;; 1/2, (1/2)/3 - 0.16666666
(fold-right list nil (list 1 2 3))
;; (list 3 nil) , (list 2 (list 3 nil)),
;; (list 1 (list 2 (list 3 nil)))
;; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;; (list nil 1), (list (list nil 1) 2),
;; (list (list (list nil 1) 2) 3)

;;if op is commutative fold-left is equivalent to fold-right



