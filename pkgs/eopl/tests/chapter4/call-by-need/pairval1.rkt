#lang eopl

(require "store.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;; mutable pairs ;;;;;;;;;;;;;;;;

;; represent a mutable pair as two references.

;; Page: 124
(define-datatype mutpair mutpair? 
  (a-pair
   (left-loc reference?)
   (right-loc reference?)))

;; make-pair : ExpVal * ExpVal -> MutPair
;; Page: 124
(define make-pair
  (lambda (val1 val2)
    (a-pair
     (newref val1)
     (newref val2))))

;; left : MutPair -> ExpVal
;; Page: 125
(define left
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (deref left-loc)))))

;; right : MutPair -> ExpVal
;; Page: 125
(define right
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (deref right-loc)))))

;; setleft : MutPair * ExpVal -> Unspecified
;; Page: 125
(define setleft
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (setref! left-loc val)))))

;; setright : MutPair * ExpVal -> Unspecified
;; Page: 125
(define setright
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (setref! right-loc val)))))


