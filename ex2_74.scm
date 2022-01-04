#lang sicp

;; key value helpers
(define (kvp k v)
  (cons k v))

(define (key kvp)
  (car kvp))

(define (value kvp)
  (cdr kvp))

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

(define (match-kvp l k v)
  (define (match-kvp-ind l k v n)
    (if (null? l) '()
        (let ((candidate-kvp (car l)))
          (cond ((and (eq? k (key candidate-kvp)) (eq? v (value candidate-kvp))) n)
                (else (match-kvp-ind (cdr l) k v (+ n 1))))) ))
  (match-kvp-ind l k v 1))

(define (match-item l v)
  (define (match-item-ind l v n)
    (if (null? l) '()
        (cond ((eq? (car l) v) n)
              (else (match-item-ind (cdr l) v (+ n 1))))))
  (match-item-ind l v 1))

;; datasets

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
                  (eq? (cadr record) type)) (caddr record))
            (else (get-registry op type (cdr registry))))))
  (get-registry op type method-registry))

;; a get-record
;; we need to make the records typed to we can dispatch a generic procedure.

(define it-personnel-typed
  (cons 'it it-personnel))

(define hr-personnel-typed
  (cons 'hr hr-personnel))

;; generic
(define (get-record file name)
  ((get 'get-record (car file)) (cdr file) name))

(define (get-record-it personnel-record name)
  (let ((record-ind (match-kvp (get-elem-by-key personnel-record 'keys) 'name name)))
    (if (null? record-ind)
        '()
        (cons (get-elem-n (get-elem-by-key personnel-record 'keys) record-ind)
          (get-elem-n (get-elem-by-key personnel-record 'values) record-ind)))))

(put 'get-record 'it get-record-it)

(define (get-record-hr personnel-record name)
  (define (scan-records db k v)
    (if (null? db)
        '()
        (let ((row (car db)))
          (let ((key-ind (match-item (get-elem-by-key row 'keys) k)))
            (if (eq? (get-elem-n (get-elem-by-key row 'values) key-ind)  v)
                row
                (scan-records (cdr db) k v))))))
  (define (zip keys values)
    (map cons keys values))
  (let ((match (scan-records personnel-record 'name name)))
    (if (not (null? match))
        (zip
         (get-elem-by-key match 'keys)
         (get-elem-by-key match 'values))
        '())))


(put 'get-record 'hr get-record-hr)

(get-record it-personnel-typed "Mike")

(get-record it-personnel-typed "Jessie")

(get-record it-personnel-typed "Nobody")

(get-record hr-personnel-typed "Bob")

(get-record hr-personnel-typed "Julia")

(get-record hr-personnel-typed "Nobody")

;; b get-salary
;; pretty easy since we have underlying generics that return data in the same form

;; the salary keys differ between databases this takes the salary key name as an arg
(define (get-salary-it personnel-file name)
  (let ((record (get-record-it personnel-file name)))
    (if (null? record)
        '()
        (get-elem-by-key record 'salary))))

(define (get-salary-hr personnel-file name)
  (let ((record (get-record-hr personnel-file name)))
    (if (null? record)
        '()
        (get-elem-by-key record 'pay))))

(define (get-salary personnel-file name)
  ((get 'get-salary (car personnel-file)) (cdr personnel-file) name))

(put 'get-salary 'it get-salary-it)

(put 'get-salary 'hr get-salary-hr)

(get-salary it-personnel-typed "Mike")

(get-salary hr-personnel-typed "Bob")

;; c find-employee-record

(define hr-files (list it-personnel-typed hr-personnel-typed))

(define (find-employee name files)
  (if (null? files)
      '()
      (let ((match (get-record (car files) name)))
        (if (not (null? match))
            match
            (find-employee name (cdr files))))))

(find-employee "Julia" hr-files)

;;d

;; When Insatiable takes over a new company they need to implement the get-record interface for the new company's data files. They can be added to the list of hr files.
