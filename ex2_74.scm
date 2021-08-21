#lang sicp
(define (make-person-hr name position pay location)
  (list (cons 'keys (list 'name 'position 'pay 'location))
        (cons 'values (list name position pay location))))

(define hr-personnel
  (list (make-person-hr "Bob" "Chief People Officer" 100000 "SF")
        (make-person-hr "Julia" "Form Desinger" 40000 "SF")))

(define (make-person-it name position salary address)
  (cons
    (cons 'name name)
  (list
   (cons 'address address)
   (cons 'position position)
   (cons 'salary salary))))

(define it-personnel
  (let ((records (list (make-person-it "Mike" "Hacker" 100000 "BRI")
                       (make-person-it "Jessie" "Programmer" 200000 "BRI"))))
    (list
     (cons 'keys (map (lambda (x) (car x)) records))
     (cons 'values (map (lambda (x) (cdr x)) records)))))

it-personnel

hr-personnel

; homebrew registry

(define method-registry '())

(define (put op type item)
  (set! method-registry (cons (list op type item) method-registry )))

(define (get op type)
  (define (get-registry op type registry)
    (let ((record (car registry)))
      (cond ((and (eq? (car record) op)
                  (eq? (cadr record) type)) (caddr record))
            (else (get-registry op type (cdr registry))))))
  (get-registry op type method-registry))

;; a get-record
;; first thing is we need to make the records typed to we can dispatch a generic procedure.

(define it-personnel-typed
  (cons 'it it-personnel))

(define hr-personnel-typed
  (cons 'hr hr-personnel))

(define (get-record file name)
  ((get 'get-record (car file)) (cdr file) name))

(define (get-record-it))
