;; I've seen this already in exercise 20.
;; If you try to build a list with an iterative process you hit two awkward situations.
;; 1. if you try to do (cons result result-set) where result-set starts out as '():
;;   Then n+1th result gets cons'd onto the front of the nth building out a result set
;;   that is in the reverse order to the source list you started with.
;;
;; 2. if you try to do (cons result-set result) then you're not going to end up with something
;;  that is a proper list structure. Since you can cons lists and single elems. You're doing:
;;  ((((() . n) . n+1) n+2) ...) which is not what you wanted.
