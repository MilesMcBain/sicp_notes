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


(define (cons2 a b)
  (* (fast-exp 2 a)
     (fast-exp 3 b)))

(define (n-divides n a)
  (define (iter n a counter)
    (cond ((= (remainder n a) 0) (iter (/ n a) a (+ counter 1)))
          (else counter)))
  (iter n a 0))

(define (car2 pair)
  (n-divides pair 2))

(define (cdr2 pair)
  (n-divides pair 3))

(define tst (cons2 3984 66))

(car2 tst)

(cdr2 tst)


