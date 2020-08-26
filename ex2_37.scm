#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; in scheme map is a parallel map

(dot-product (list 1 2 3) (list 1 1 1))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v)) m))

(define mat (list (list 1 2 3 4)
                  (list 4 5 6 6)
                  (list 6 7 8 9)))

(define v1 (list 1 1 1 1))

(matrix-*-vector mat v1)

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose mat)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
  (transpose (map (lambda (column)
                    (matrix-*-vector m column)) cols))))

(define I (list (list 2 0 0)
                (list 0 1 0)
                (list 0 0 1)))

(define V (list 1 4 7))

(define M (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)))

(matrix-*-vector I V)

(matrix-*-matrix I M)

(define (matrix-*-matrix2 m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))
;; I think this is the only way to do it their way,
;; with an outer map over m.
;; perhaps a little clearer what is going on than the
;; original which has a the mysterious outer transpose.

(matrix-*-matrix2 I M)
