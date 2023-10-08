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
                (eq? (cadar registry-to-search) type))
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

;; defining =zero?
(define (=zero? arg1)
  (get '=zero? (type-tag arg1)))

(define (=zero?-number arg1)
  (= 0 arg1))

(define (=zero?-rational arg1)
  (= 0 (numer arg1)))

(define (=zero?-complex arg1)
  (and (= 0 (real-part arg1))
       (= 0 (imaginary-part arg1))))


;; installing the generic methods

(put '=zero? 'scheme-number =zero?-number)

(put '=zero? 'rational =zero?-rational)

(put '=zero? 'complex =zero?-complex)
