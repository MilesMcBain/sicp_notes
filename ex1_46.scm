#lang sicp

(define (iterative-improve improve-guess eval-guess)
  (define (improve x improve-guess eval-guess)
    (let ((next-guess (improve-guess x)))
      (cond ((eval-guess x next-guess) x)
            (else (improve next-guess improve-guess eval-guess)))))
  (lambda (x) (improve x improve-guess eval-guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-avg x)
  ((iterative-improve
   (lambda (guess) (average guess (/ x guess)))
   (lambda (guess1 guess2) (< (abs (- guess1 guess2)) 0.00001)))
   1.0))

(sqrt-avg 25)

(define (fixed-point f first-guess)
  ((iterative-improve
   f
   (lambda (guess1 guess2) (< (abs (- guess1 guess2)) 0.00001)))
   first-guess))

(fixed-point cos 1.0)
