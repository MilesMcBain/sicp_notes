#lang sicp

;; an iterative version of sum
(define (sum term a next b)
  (define (sum-iter a result)
   (if (> a b)
       result
       (sum-iter (next a)
                 (+ result (term a)))
       )
   )

  (sum-iter a 0)
 )

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

(define (next-cube x)
  (+ x 1))

(sum cube 1 next-cube 5)
