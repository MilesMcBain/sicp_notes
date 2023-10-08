;; a

;; If we try to call `exp` with two `complex` args we will get stuck in an
;; infinite loop of type coercion to `complex` and attempted dispatch of the
;; generic function.
;;
;; (exp) will result in a new attempted call to (exp) post coercion of type 1 (complex) to type 2 (complex). The new call will resolve the same way ad infinitum.

;; b

;; `apply-generic` worked okay as-is. In the case that a method cannot be found
;; and both args are the same type, then without self-coercion no coercion
;; option is found and so no further attempt is made to dispatch the function.

;; However, it is perhaps surprising to the user that they can catastrophically
;; break the generic dispatch mechanism by defining a self-coercion. This should be
;; guarded against int he implementation of apply-generic.

;; c

;; An if expression has been added to capture the case where the types are equal.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (not (equ? type1 type 2))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2-t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags)))))
                (error "no method for these types"
                       (list op type-tags))))
          (error "No method for these types"
                 (list op type-tags))))))
