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

(define (reverse sequence)
  (fold-right (lambda (x y)
                (if (null? y)
                    (list x)
                    (append y (list x)))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse (list 1 2 3))
(reverse2 (list 1 2 3))
