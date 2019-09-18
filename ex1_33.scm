#lang sicp

; copy pasta from ex1_18
(define (divides? n d)
  (= (remainder n d) 0))

(define (square x)
  (* x x))

(define (smallest-divisor n)

  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))

  (define (divisor-search n divisor)
    (cond ((> (square divisor) n) n)
          ((divides? n divisor) divisor)
          (else (divisor-search n (next divisor)))))

  (divisor-search n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (prime? n)
  (= (smallest-divisor n) n))
; end copy pasta

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate combiner predicate null-value term a next b)

  (define (accumulate-iter a result)
    (cond ((> a b) result)
          ((predicate a) (accumulate-iter (next a)
                                          (combiner result
                                                    (term a))))
          (else (accumulate-iter (next a)
                           result))))

  (accumulate-iter a null-value))

; sum of primes
(define (inc x) (+ x 1))

(=
 (filtered-accumulate + prime? 0 square 1 inc 10)
 (+ 1 4 9 25 49)
 )

; sum of positive integers less than n that are relatively prime to n
(define (rel-prime-sum n)
  (define (relatively-prime-n? a)
    (= (gcd n a)
       1))
  (filtered-accumulate + relatively-prime-n? 0 identity 1 inc (- n 1)))

(=
 (rel-prime-sum 10)
 (+ 1 3 7 9)
 )
