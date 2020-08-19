#lang sicp

(define (deep-reverse l)
  (define (reverse-itr l rev-l)
    (cond ((null? l) rev-l)
          ((pair? (car l)) (reverse-itr (cdr l)
                                        (cons (deep-reverse (car l)) rev-l)))
          (else (reverse-itr (cdr l)
                             (cons (car l) rev-l)))))

  (reverse-itr l nil))

(define x (list (list 1 2) (list 3 4)))

(cdr x)
(car x)

(deep-reverse (car x))
(deep-reverse x)

;; what about a recursive implementation?


(reverse (list 5 6 7 8))

(define (deep-reverse2 l)
  (cond ((null? l) nil)
        ((pair? l) (append (deep-reverse2 (cdr l))
                           (list (deep-reverse2 (car l)))))
        (else l)))

(deep-reverse2 x)

;; car unwraps one level of listing, cdr does not.
;; So if we want to preserve the nestedness we have to (list) the (car)
;; eg x = (list (list 1 2) (list 3 4))
;; (append (cdr x) (car x))
;; ((3 4) 1 2)
;;
;; (append (cdr x) (list (car x)))
;; ((3 4) (1 2))
