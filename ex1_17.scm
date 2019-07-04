#lang sicp

(define (divides? n d)
  (= (remainder n d) 0))

(define (square x)
  (* x x))

(define (smallest-divisor n)

  (define (divisor-search n divisor)
    (cond ((> (square divisor) n) n)
          ((divides? n divisor) divisor)
          (else (divisor-search n (+ divisor 1)))))

  (divisor-search n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (let*
      ((start-time (runtime))
      (found-prime? (prime? n))
      (elapsed-time (- (runtime) start-time)))
    (cond (found-prime?
           (write " *** ")
           (write n)
           (write "|")
           (write elapsed-time)
           #t)
          (else #f))))


(define (search-for-primes lower upper)
           
  
  (let
      ((lower (cond ((even? lower) (+ lower 1))
                    (else lower)))
       (upper (cond ((even? upper) (- upper 1))
                    (else upper))))

    (define (prime-hunter n primes)
      (cond ((= primes 3)1)
            ((> n upper)0)
            ((timed-prime-test n) (prime-hunter (+ n 2) (+ primes 1)))
            (else (prime-hunter (+ n 2) primes))))

    (prime-hunter lower 0)))

(search-for-primes 1000 2000)

(search-for-primes 10000 20000)

(search-for-primes 1000000 2000000)