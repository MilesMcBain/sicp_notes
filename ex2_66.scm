#lang sicp

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      '()
      (let ((record-entry-key (key (entry set-of-records))))
        (cond ((equal? given-key record-entry-key) (entry set-of-records))
              ((> given-key record-entry-key (lookup given-key (left-branch set-of-records))))
              (else (lookup given-key (right-branch set-of-records)))))))
