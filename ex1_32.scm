#lang sicp

(define (accumulate combiner null-value term a next b)

  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a)
                        (combiner result
                                  (term a)))))

  (accumulate-iter a null-value))

(define (inc x) (+ x 1))

(define (identity x) x)

(=
 (accumulate * 1 identity 1 inc 5)
 120)

(=
 (accumulate + 0 identity 1 inc 5)
 15)

; recursive version
(define (accumulate-recursive combiner null-value term a next b)

  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner
                                      null-value
                                      term
                                      (next a)
                                      next
                                      b)))
  )

(=
 (accumulate-recursive * 1 identity 1 inc 5)
 120)

(=
 (accumulate-recursive + 0 identity 1 inc 5)
 15)
