#lang sicp

(define (divides? n d)
  (= (remainder n d) 0))

(define (square x)
  (* x x))


(define (even? n)
  (= (remainder n 2) 0))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (list-contains? l item)
  (not (equal? (member item l) #f))
  )

(define (sample-less-than-not-in n checked-nums)

  (define (sample-itr n checked-nums)
    (let ((sample (+ 2 (random (- n 2)))))
      (if (list-contains? checked-nums sample)
          (sample-itr n checked-nums)
          sample)))

  (sample-itr n checked-nums))


(define (fast-prime? n n-tests)

  (define (fermat-iter n n-tests checked-nums)
    (if (= n-tests 0)
        #t
        (let ((a (sample-less-than-not-in n checked-nums)))
              (if (= (expmod a n n) a)
                  (fermat-iter n (- n-tests 1) (append checked-nums (list a)))
                  #f))))

  (fermat-iter n n-tests (list)))

;; Don't check to see we haven't sampled the number before
;; Unsafe since it could affect changes of spurious prime
(define (unsafe-fast-prime? n n-tests)

  (define (fermat-iter n n-tests)
    (if (= n-tests 0)
        #t
        (let ((a (+ 2 (random (- n 2)))))
          (if (= (expmod a n n) a)
              (fermat-iter n (- n-tests 1))
              #f))))

  (fermat-iter n n-tests ))



(define (timed-prime-test n)
  (let*
      ((start-time (runtime))
      (found-prime? (fast-prime? n 8))
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
      (cond ((= primes 3) 1)
            ((> n upper) 0)
            ((timed-prime-test n) (prime-hunter (+ n 2) (+ primes 1)))
            (else (prime-hunter (+ n 2) primes))))

    (prime-hunter lower 0)))

(search-for-primes 1000 2000)

(search-for-primes 10000 20000)

(search-for-primes 1000000 2000000)
