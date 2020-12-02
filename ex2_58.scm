#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

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

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))))
  )

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (accumulate make-sum 0 (cddr s)))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (accumulate make-product 1 (cddr p)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (exponentiation? e) #f)

(define (base e) (cadr e))

(define (exponent e) (caddr e))


(deriv '(x + 2) 'x)

(deriv '(x * y) 'x)

(deriv '(x * y * (x + 3)) 'x)

(deriv '(x + 3 * (y + 2)) 'x)

;; everything is a sum if it contains '+', only if no '+' may '*' be considered.
;; use memq on p195 to break up lists according to presence of symbols.
