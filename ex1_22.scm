#lang sicp

(define (divides? n d)
  (= (remainder n d) 0))

(define (square x)
  (* x x))


(define (even? n)
  (= (remainder n 2) 0))


(define (list-contains? l item)
  (not (equal? (member item l) #f))
  )

(define (sample-less-than-not-in n checked-nums)

  (define (sample-itr n checked-nums)
    (let ((sample (+ 2 (random (- n 2)))))
      (if (or (list-contains? checked-nums sample)
              (not (= 1 (gcd n sample))))
          (sample-itr n checked-nums)
          sample)))

  (sample-itr n checked-nums))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (jacobi-symbol a n)
  (cond ((= a 1)
         1)
        ((even? a)
         (* (jacobi-symbol
             (/ a 2) n)
            (expt -1 (/ (- (square n) 1)
                        8))))
        (else
         (* (jacobi-symbol (remainder n a) a)
            (expt -1 (* (- a 1)
                        (/ (- n 1)
                           4)))))))

(define (soloway-strassen-test n k)

  (define (sstest-iter n k checked-nums)
    (cond ((= k 0)
           #t)
          (else
           (let ((a (sample-less-than-not-in n checked-nums)))
             (if (= (expmod a (/ (- n 1) 2) n)
                    (modulo (jacobi-symbol a n) n))
                 (sstest-iter n (- k 1) (append checked-nums (list a)))
                 #f
                 )
              )
           ))
    )

  (sstest-iter n k (list))

)



(soloway-strassen-test 11 3)

(soloway-strassen-test 199 3)

(soloway-strassen-test 1999 3)
