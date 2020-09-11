#lang sicp

;; prerequisites
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval lower upper)
  (if (> lower upper)
      nil
      (cons lower (enumerate-interval  (+ lower 1) upper))))


;; given queens code
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        empty-board
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board (list nil))

(define (position row col)
  (list row col))

(define (row position)
  (car position))

(define (col position)
  (cadr position))

;; we store the positions in reverse order this makes life easier later
;; i.e. to compare a new position with all others the new position is the
;; car of the list of positions, while the rest of the positions to be compared
;; are the cdr.

(define (append-position position-list position)
  (append (list position) position-list)
  )

(define (adjoin-position row col positions)
  (if (null? positions)
      (list (position row col))
      (append-position positions (position row col))))

(define (last-column positions)
  (car positions))

(define (columns-before-last positions)
  (cdr positions))

(define (safe-from? position-b position-a)
  (and
   (not (= (row position-b) (row position-a)))
   (not (= (+ (row position-b) (col position-b))
           (+ (row position-a) (col position-a))))
   (not (= (- (row position-b) (col position-b))
           (- (row position-a) (col position-a))))))

;; this version of safe doesn't use the column
;; although it could be rewritten to use that if desired
;; last-column -> kth
;; columns-before-last -> k-1-cols
;; some what neatly when sequence is nil, accumulate returns initial
;; in this case TRUE. This will happen when there is only 1 position
(define (safe? column positions)
  (let ((candidate (last-column positions)))
    (accumulate (lambda (queen-position safety-indicator)
                  (and
                   safety-indicator
                   (safe-from? candidate queen-position)))
                #t
                (columns-before-last positions))))


;; testing
(adjoin-position 2 2 (list (list (position 1 1)) (list (position 2 1))))

(append-position (list (position 1 1)) (position 2 2))

(safe? 3 (list (position 2 1)
               (position 4 2)
               (position 1 3)
               (position 3 4)))

(safe? 3 (list (position 1 4)
               (position 1 3)
               (position 1 2)
               (position 1 1)))

(safe-from? (position 1 2) (position 1 1))

(safe? 1 (list (list 1 2) (list 1 1)))

(filter
 (lambda (positions) (safe? 1 positions))
 (list (list (position 1 2)
             (position 1 1))
       (list (position 2 3)
             (position 1 1))))

(queens 8)
