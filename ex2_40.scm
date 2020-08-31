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


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval lower upper)
  (if (> lower upper)
      nil
      (cons lower (enumerate-interval  (+ lower 1) upper))))


(define (unique-pairs n)
  (flat-map (lambda (x)
              (map (lambda (y) (list y x)) (enumerate-interval 1 (- x 1))))
              (enumerate-interval 1 n)))


(define (pair-sum pair)
  (+ (car pair) (cadr pair)))

(define (make-pair-sum-summary pair)
  (list (car pair) (cadr pair) (pair-sum pair)))

(define (prime-sum? pair)
 (prime? (pair-sum pair)))

(define (prime-sum-pairs n)
  (map make-pair-sum-summary
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 5)
