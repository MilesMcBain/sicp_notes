#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols-tree tree) (caddr tree))
(define (weight-tree tree) (cadddr tree))

(define (symbols element)
  (if (leaf? element)
      (list (symbol-leaf element))
      (symbols-tree element)))
(define (weight element)
  (if (leaf? element)
      (weight-leaf element)
      (weight-tree element)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (list-contains? l sym)
  (cond ((null? l) false)
        ((eq? sym (car l)) true)
        (else (list-contains? (cdr l) sym))))

(define (encode-symbol char tree)
  (define (make-bits char tree bit-string)
    (cond ((leaf? tree) bit-string)
          ((list-contains? (symbols (left-branch tree))
                           char)
           (make-bits char
                      (left-branch tree)
                      (append bit-string (list 0))))
          ((list-contains? (symbols (right-branch tree)) char)
           (make-bits char
                      (right-branch tree)
                      (append bit-string (list 1))))
          (else (error "I got lost in the tree."))))
  (if (not (list-contains? (symbols tree) char))
      (error "I can't find symbol " char " in tree.")
      (make-bits char tree '())))

(define (generate-huffman-tree pairs)
  (successive-merge(make-leaf-set pairs)))

(define (successive-merge node-list)
  (if (null? (cdr node-list))
      (car node-list) ; There's one node in the list - the tree!
      (successive-merge
       (adjoin-set (make-code-tree
                    (car node-list)
                    (cadr node-list))
                   (cddr node-list)))))


(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define generated-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))) )

sample-tree
generated-tree

(decode sample-message generated-tree)
