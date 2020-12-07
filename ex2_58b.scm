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
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

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

(define (split-sym l sym)
  (define (split-it lhs rhs sym)
    (cond ((null? rhs) nil)
          ((eq? (car rhs) sym) (list lhs (cdr rhs)))
          (else (split-it(append lhs (list (car rhs))) (cdr rhs) sym))
          ))

  ;; This function is needed because a single element list containing a
  ;; variable or number isn't a data type that is understood by the rest of the
  ;; program. It unwraps length 1 lists to give the atomic element.
  (define (simplify-splits l)
    (map (lambda (split)
           (if (and (not (null? split)) (null? (cdr split)))
               (car split)
               split))
         l))

  (simplify-splits (split-it '() l sym)))

(define (contains-sym? l sym)
  (cond ((null? l) #f)
        ((eq? (car l) sym) #t)
        (else (contains-sym? (cdr l) sym))))

(define (odd? x) (= (remainder x 2) 1))

(define (operators l)
  (define (op-it idx l result)
    (cond ((null? l) result)
          ((odd? idx) (op-it (+ idx 1)
                             (cdr l)
                             (append result (list (car l)))))
          (else (op-it (+ idx 1)
                       (cdr l)
                       result))))
  (op-it 0 l '()))

(define (sum? x) (and (pair? x) (contains-sym? (operators x) '+)))

(define (addend s) (car (split-sym s '+)))

(define (augend s) (cadr (split-sym s '+)))

(define (product? x) (and (pair? x)
                          (contains-sym? (operators x) '*)))

(define (multiplier p) (car (split-sym p '*)))

(define (multiplicand p) (cadr (split-sym p '*)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (exponentiation? e) #f)

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(deriv '(2 + x) 'x)

(deriv '(x * y) 'x)

(deriv '(x * y * (x + 3)) 'x)

(deriv '(x + 3 * (y + 2)) 'x)


;; (sum? '(2 + x))

;; (addend '(2 + x))

;; (augend '(2 + x))


;; (sum? '(2 + x + y))

;; (addend '(2 + x + y))

;; (augend '(2 + x + y))

