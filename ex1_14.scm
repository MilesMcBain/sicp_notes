#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))
  
(define (fast-exp b n)

  (define (iter b n a)
    (cond ((= n 0) a)
          ((not (even? n)) (iter b (- n 1) (* a b)))
          (else (iter (square b) (/ n 2) a))))
  (iter b n 1))

(define (cheat-fib n)
  (let ((theta (/ (+ 1 (sqrt 5)) 2)))
    (cond ((= n 0) 1)
          ((= n 1) 1)
          ((even? n) (floor (/ (fast-exp theta n) (sqrt 5))))
          (else (ceiling (/ (fast-exp theta n) (sqrt 5)))))))

(cheat-fib 1)
(cheat-fib 2)
(cheat-fib 3)
(cheat-fib 4)
(cheat-fib 5)
(cheat-fib 6)
(cheat-fib 7)