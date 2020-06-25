#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((pos-x-lower (positive? (lower-bound x)))
        (pos-x-upper (positive? (upper-bound x)))
        (pos-y-lower (positive? (lower-bound y)))
        (pos-y-upper (positive? (upper-bound y))))
    (cond ((and pos-x-lower pos-x-upper
                pos-y-lower pos-y-upper)
           ;; all pos
           (make-interval (* (lower-bound x) (lower-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (not pos-x-lower) (not pos-x-upper)
                (not pos-y-lower) (not pos-y-upper))
           ;; all neg
           (make-interval (* (upper-bound x) (upper-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (not pos-x-lower) (not pos-x-upper)
                pos-y-lower pos-y-upper)
           ;; x neg, y pos
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (upper-bound x) (lower-bound y))))
          ((and pos-x-lower pos-x-upper
                (not pos-y-lower) (not pos-y-upper))
           ;; x pos, y neg
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (lower-bound x) (upper-bound y))))
          ((and (not pos-x-lower) pos-x-upper
                 pos-y-lower pos-y-upper)
           ;; x split, y pos
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and pos-x-lower pos-x-upper
                (not pos-y-lower) pos-y-upper)
           ;; x pos, y split
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (not pos-x-lower) (not pos-x-upper )
                (not pos-y-lower ) pos-y-upper)
           ;; x neg, y split
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (not pos-x-lower) pos-x-upper
                (not pos-y-lower) (not pos-y-upper))
           ;; x split, y neg
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (not pos-x-lower) pos-x-upper
                (not pos-y-lower) pos-y-upper)
           ;; x split, y split
           (let ((p1 (* (lower-bound x) (lower-bound y)))
                 (p2 (* (lower-bound x) (upper-bound y)))
                 (p3 (* (upper-bound x) (lower-bound y)))
                 (p4 (* (upper-bound x) (upper-bound y))))
             (make-interval (min p1 p2 p3 p4)
                            (max p1 p2 p3 p4)))))))

(define (div-interval x y)
  (cond ((and (< (lower-bound y) 0) (> (upper-bound y) 0)) ;; ex 2.10
         (newline)
         (display "Can't divide by an interval spanning 0!"))
        (else (mul-interval
               x
               (make-interval (/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y)))))))

(define (make-interval a b) (cons a b))

;; ex 2.7
(define (upper-bound int)
  (cdr int))

(define (lower-bound int)
  (car int))

;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))

;; ex 2.9
;; see notes

;; ex 2.11
(mul-interval (make-interval -2 3)
              (make-interval 1 4))

(mul-interval (make-interval -4 -1)
              (make-interval 1 4))

(mul-interval (make-interval -4 3)
              (make-interval -5 8))
