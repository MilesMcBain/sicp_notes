#lang sicp

; type infrastructure

(define (type-tag x)
  (car x))

(define (contents x)
  (cdr x))


; homebrew generic registry

(define (list-eq? list1 list2)
  (cond ((not (= (length list1) (length list2))) false)
        ((and (null? list1) (null? list2)) true)
        ((not (eq? (car list1) (car list2))) false)
        (else (list-eq? (cdr list1) (cdr list2)))))

(define (type-eq? types1 types2)
  (cond ((not (eq? (symbol? types1) (symbol? types2))) false)
        ((symbol? types1) (eq? types1 types2))
        ((list? types1) (list-eq? types1 types2))))


(define (make-method-registry)
  (define method-registry '())

  (define local-methods
    (list
     (cons 'put (lambda (op type item)
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
       (itr-registry op type '() method-registry)))
     (cons 'get (lambda (op type)
       (define (get-registry op type registry)
         (let ((record (car registry)))
           (cond ((and (eq? (car record) op)
                       (type-eq? (cadr record) type)) (caddr record))
                 (else (get-registry op type (cdr registry))))))
       (get-registry op type method-registry)))))

  (lambda (sym)
    (define (get-method-iter searched)
      (cond ((null? searched) (error "method not found: " sym))
            ((eq? (caar searched) sym) (cdr (car searched)))
            (else (get-method-iter (cdr searched)))))
  (get-method-iter local-methods)))

(define generic-registry (make-method-registry))

(define put (generic-registry 'put))

(define get (generic-registry 'get))

(put (lambda (x) (print x)) 'print 'scheme-number)

(get 'print 'scheme-number)


;; 2.82 a version that tries to coerce to same type of successive args until it finds a match
(define (all-eq? l-args)
  (define (equ-iter current remains)
    (cond ((null? remains) #t)
          ((eq? current (car remains))
           (equ-iter (car remains) (cdr remains)))
          (else #f)))
  (equ-iter (car l-args) (cdr l-args)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (all-eq? type-tags))
                   ;; do conversion
                   (error "no method for these types"
                          (list op type-tags)))))))


          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (not (all-eq? type-tags))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2-t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags)))))
                (error "no method for these types")
                       (list op type-tags)))))))

;; 
