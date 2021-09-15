#lang sicp

;; key value helpers
(define (kvp k v)
  (cons k v))

(define (key kvp)
  (car kvp))

(define (value kvp)
  (cdr kvp))

;; get the nth element
(define (get-elem-n l n)
  (cond ((= n 0) '())
        ((null? l) (error "get-elem-n: index out of bounds"))
        ((= n 1) (car l))
        (else (get-elem-n (cdr l) (- n 1)))))

(define (get-elem-by-key l k)
  (if (null? l) (error "get-elem-with-key: no match for key")
      (let ((candidate-elem (car l)))
        (cond ((eq? (key candidate-elem) k) (cdr candidate-elem))
              (else (get-elem-by-key (cdr l) k))))))

(define (match-kvp k v l)
  (define (match-kvp-ind k v l n)
    (if (null? l) '()
        (let ((candidate-kvp (car l)))
          (cond ((and (eq? k (key candidate-kvp)) (eq? v (value candidate-kvp))) n)
                (else (match-kvp-ind k v (cdr l) (+ n 1))))) ))
  (match-kvp-ind k v l 1))

(define (make-person-hr name position pay location)
  (list (cons 'keys (list 'name 'position 'pay 'location))
        (cons 'values (list name position pay location))))

(define hr-personnel
  (list (make-person-hr "Bob" "Chief People Officer" 100000 "SF")
        (make-person-hr "Julia" "Form Desinger" 40000 "SF")))

(define (make-person-it name position salary address)
  (cons
    (kvp 'name name)
  (list
   (kvp 'address address)
   (kvp 'position position)
   (kvp 'salary salary))))

(define it-personnel
  (let ((records (list (make-person-it "Mike" "Hacker" 100000 "BRI")
                       (make-person-it "Jessie" "Programmer" 200000 "BRI"))))
    (list
     (kvp 'keys (map (lambda (x) (car x)) records))
     (kvp 'values (map (lambda (x) (cdr x)) records)))))

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

;; generic
(define (get-record file name)
  ((get 'get-record (car file)) (cdr file) name))

(define (get-record-it personnel-record name)
  (let ((record-ind (match-kvp 'name name (get-elem-by-key personnel-record 'keys))))
    (cons (get-elem-n (get-elem-by-key personnel-record 'keys) record-ind)
          (get-elem-n (get-elem-by-key personnel-record 'values) record-ind))))

(put 'get-record 'it get-record-it)

(get-record it-personnel-typed "Mike")
(get-record it-personnel-typed "Jessie")
