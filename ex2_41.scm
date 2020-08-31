#lang sicp

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


(define (unique-tripples n)
  (flat-map (lambda (x)
              (flat-map (lambda (y)
                     (map (lambda (z) (list z y x))
                          (enumerate-interval 1 (- y 1))))
                   (enumerate-interval 1 (- x 1))))
              (enumerate-interval 1 n)))

(define (sum-tripple tripple)
  (+ (car tripple) (cadr tripple) (caddr tripple)))

(define (tripple-sum-s-pred s)
  (lambda (tripple) (= s (sum-tripple tripple))))

(define (make-tripple-sum-summary tripple)
  (list (car tripple)
        (cadr tripple)
        (caddr tripple)
        (sum-tripple tripple)))

(define (tripples-under-n-that-sum-s n s)
  (map make-tripple-sum-summary
       (filter (tripple-sum-s-pred s)
               (unique-tripples n))))

(tripples-under-n-that-sum-s 10 12)
