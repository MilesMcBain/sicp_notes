#lang sicp

; dummy

(define (type-tag x)
  x)

(define (numer x) x)

(define (denom x) x)

(define (real-part x) x)

(define (imag-part x) x)

; homebrew registry

(define (list-eq? list1 list2)
  (cond ((not (= (length list1) (length list2))) false)
        ((and (null? list1) (null? list2)) true)
        ((not (eq? (car list1) (car list2))) false)
        (else (list-eq? (cdr list1) (cdr list2)))))

(define (type-eq? types1 types2)
  (cond ((not (eq? (symbol? types1) (symbol? types2))) false)
        ((symbol? types1) (eq? types1 types2))
        ((list? types1) (list-eq? types1 types2))))



(define method-registry '())

(define (put op type item)
  (define (itr-registry op type registry-searched registry-to-search)
    (cond ((null? registry-to-search)
           (set! method-registry (cons (list op type item) registry-searched)))
          ((and (eq? (caar registry-to-search) op)
                (type-eq? (cadar registry-to-search) type))
           (set! method-registry (append registry-searched
                                         (cons (list op type item)
                                               (cdr registry-to-search)))))
          (else (itr-registry op type
                              (cons (car registry-to-search) registry-searched)
                              (cdr registry-to-search)))))
  (itr-registry op type '() method-registry))

(define (get op type)
  (define (get-registry op type registry)
    (let ((record (car registry)))
      (cond ((and (eq? (car record) op)
                  (type-eq? (cadr record) type)) (caddr record))
            (else (get-registry op type (cdr registry))))))
  (get-registry op type method-registry))

;; defining equ?
(define (equ? arg1 arg2)
  (get 'equ? (list (type-tag arg1) (type-tag arg2))))

(define (equ?-number-number arg1 arg2)
  (= arg1 arg2))

(define (equ?-rational-rational arg1 arg2)
  (and (= (numer arg1) (numer arg2)) (= (denom arg1) (denom arg2))))
;; assuming already most simplified form

(define (equ?-complex-complex arg1 arg2)
  (and (= (real-part arg1) (real-part arg2)) (= (imag-part arg1) (imag-part arg2))))

(define (equ?-complex-rational arg1 arg2)
  (and (= (imag-part arg1) 0) (= (real-part arg1) (/ (numer arg2) (denom arg2)))))

(define (equ?-rational-complex arg1 arg2)
  (equ?-complex-rational arg2 arg1))

(define (equ?-complex-number arg1 arg2)
  (and (= (imag-part arg1) 0) (= (real-part arg1) arg2)))

(define (equ?-number-complex arg1 arg2)
  (equ?-complex-number arg2 arg1))

(define (equ?-rational-number arg1 arg2)
  (= (/ (numer arg1) (denom arg1)) arg2))

(define (equ?-number-rational arg1 arg2)
  (equ?-rational-number arg2 arg1))

;; installing the generic methods

(put 'equ? (list 'scheme-number 'scheme-number)
     equ?-number-number)

(put 'equ? (list 'rational 'rational)
     equ?-rational-rational)

(put 'equ? (list 'complex 'complex)
     equ?-complex-complex)

(put 'equ? (list 'complex 'rational)
     equ?-complex-rational)

(put 'equ? (list 'rational 'complex)
     equ?-rational-complex)

(put 'equ? (list 'complex 'scheme-number)
     equ?-complex-number)

(put 'equ? (list 'scheme-number 'complex)
     equ?-number-complex)

(put 'equ? (list 'rational 'scheme-number)
     equ?-rational-number)

(put 'equ? (list 'scheme-number 'rational)
     equ?-number-rational)

(type-eq? 'scheme-number 'rational)

(type-eq? 'scheme-number 'scheme-number)

(list-eq? (list 'scheme-number 'scheme-number) (list 'complex 'rational))

(get 'equ? (list 'scheme-number 'scheme-number))

(get 'equ? (list 'complex 'rational))
