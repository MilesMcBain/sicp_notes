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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (make-center-width c (* c (/ tol 100))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;; ex 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))



(display "multiplication problem:\n")
(define A (make-center-percent 1000 1))
(define B (make-center-percent 2000 3))

(define AB (mul-interval A B))

(mul-interval A B)

(percent AB)

(display "parallel issue:\n")

(percent (par1 A B))

(percent (par2 A B))

(percent (div-interval A B))

(percent (div-interval A A))

(percent (div-interval (make-interval 1 1) A))

(percent (div-interval A (make-interval 1 1)))

;; I think we can explain this due to floating point error.
;; It goes away if we don't use 1.0 in div-interval.
;; The reciprocal of A cannot be represented without truncation.
;; That makes the endpoints slightly smaller than they should be.
;; But the difference in results for par1 and par2 seems too large.

(percent (add-interval A B))

;; Ooookay so adding intervals doesn't add the percent error. It's like some kind of weighted average thing. But this means adding introduces less error than multiplying or dividing.
;; Formula 2 has more divides/multiplications but they are all involving the 1 - 1 interval which has 0% error. The only op that adds to the error is the add.
;; Formula 1 has a multiply and a divide which add error which explains why it is much higher.

;; Exercise 2.15
;; Yes. Sure. This is obvious. If you reuse an interval quantity more than once in an expression, you have paid the cost of introducing it's error again. This is the root cause - not the number of operations as mentioned above.
;; But this is only due to the naive way we process expressions. We just process individual operations without a view of the whole expression.

;; Exercise 2.16
;; It's because we don't process whole expressions. We only consider single operations. Which means in the context of an expression the uncertainty associated with all variables/operations is treated as an independent contribution. Even contributions coming from same variable appearing multiple times in different operations, which doesn't make sense.

;; Yes I think it is possible. One way: If you can capture the entire expression you can avoid the pitfall of processing each operation individually and naively. You would create an outer product of all combinations of upper and lower versions of each variable and then evaluate the expression for each combination. The min and max evaluations become the new endpoints of the interval.
;; I think it is true that realising this is not possible without metaprogramming. So I think in scheme we would need to use macros. A macro expansion that creates all the expressions in the outer product and finds the min/max result.
;; There might also be some smart heuristics that could be applied to work out which combinations actually need to be compared.
