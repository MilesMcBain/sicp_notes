#lang sicp

;; a. We can't assimilate the predicates number? and variable? into our dispatch scheme
;; since they work on primitive data types. We'd need to make new constructors for
;; variables and numbers that tagged the data with these types.
;;
;; Ideally our dispatch system would work similarly for built-in types as user-created.
;; So we need a generic way of extracting the type info.

;; b

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 0) 0)
        ((=number? base 1) 1)
        (else (list '^^ base exponent))))

(define (addend s) (car s))

(define (augend s) (cadr s))

(define (multiplier p) (car p))

(define (multiplicand p) (cadr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))

(define (operarnds exp) (cdr exp))

(define (deriv-sum ops)
  (make-sum (deriv (addend ops))
            (deriv (augend ops))))

(define (deriv-product ops)
  (make-sum (make-product (multiplier p)
                          (deriv (multiplicand p)))
            (make-product (deriv (multiplier p))
                          (multiplicand p))))

(define (deriv-exponentiation ops)
  (make-product (make-product (exponent ops)
                              (make-exponent (base ops)
                                             (- (exponent ops) 1)))
                (deriv (base ops))))

(put 'deriv '(+) deriv-sum)
(put 'deriv '(*) deriv-product)
(put 'deriv '(^^) deriv-exponent)


(deriv '(+ (* 2 x) 1 'x))

;; d
;; This is hard to think about. So how would you express a system to derive?
;; (+ (* 2 x) 1) for example needs to have tagged data. Okay:
;; '(+ (deriv (* 2 x)) (deriv 1))
;; yuck.
;; And what happens if one operand is tagged as deriv, but the other is not?
;; Only have a dispatch rule for both tagged as deriv.
;; For products this is super awkward to use since a product of derivatives is not equal to a derivative of products.
;;
;; So what I think would happen is that you need to write the expression like this:
;; (make-sum (make-product 2 x) 1)
;;
;; which would make a fully tagged structure like:
;;
;; '(deriv (+ (deriv (product (deriv 2) (deriv x)))
;;            (deriv 1))
;;
;; To derive you peel off the first tag and evaluate the list
;;
;; the implementation for + looks a little like the old implementation of make-sum

(define (+-deriv a1 a2)
  (cond  ((and (number? a1) (number? a2)) 0)
         ((number? a1) a2)
         ((number? a2) a1)
         (else (make-sum a1 a2))))

;; not correct - needs an 'eval' on each returned thing to derive it.

