#lang sicp

(define (for-each fn l)
  (if (null? l)
      #t
      ((lambda ()
        (fn (car l))
        (for-each fn (cdr l))))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
