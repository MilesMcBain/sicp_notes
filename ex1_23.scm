#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (even? x)
  (= (remainder x 2) 0))

(define (cube x)
  (* x x x))

(define (simpsons-approx fn a b n)
  (let ((h (/ (- b a) n))
        )
    (define (simp-term y)
      (if (even? y)
          (* 2 (fn (+ a (* y h))))
          (* 4 (fn (+ a (* y h))))
          ))
    (define (simp-next y)
      (+ y 1))
    (*  (/ h 3)
        (+ (fn a)
           (fn (+ a b))
           (sum simp-term 1 simp-next  (- n 1))))
    ))

(simpsons-approx cube 0 1 4)
(simpsons-approx exp 0 1 4)
