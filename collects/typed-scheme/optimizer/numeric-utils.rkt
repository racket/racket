#lang racket/base

(require syntax/parse racket/dict syntax/id-table
         (for-template racket/base racket/flonum racket/fixnum racket/unsafe/ops)
         "../utils/utils.rkt"
         (types numeric-tower)
         (optimizer utils))

(provide (all-defined-out))

;; layer predicates
;; useful in some cases where subtyping won't do
(define (in-integer-layer? t)
  (subtypeof? t -Int))
(define (in-rational-layer? t)
  (and (subtypeof? t -Rat)
       (not (subtypeof? t -Int))))
(define (in-float-layer? t)
  (subtypeof? t -Flonum))
(define (in-real-layer? t)
  (and (subtypeof? t -Real)
       (not (subtypeof? t -Rat))
       (not (subtypeof? t -Flonum))))
(define (in-complex-layer? t)
  (and (subtypeof? t -Number)
       (not (subtypeof? t -Real))))

(define-syntax-class arith-expr
  (pattern (#%plain-app op:arith-op args ...)))
(define-syntax-class arith-op
  (pattern
   op:id
   #:when (dict-ref arith-ops #'op (lambda () #f))))
;; limited to operation that actually perform arithmeric
;; so, no comparisons, or coercions, or constructors (make-rectangular), accessors, etc.
(define arith-ops
  (make-immutable-free-id-table
   (map (lambda (x) (list x #t))
        (list #'* #'+ #'- #'/
              #'max #'min #'add1 #'sub1
              #'quotient #'remainder #'modulo
              #'arithmetic-shift #'bitwise-and #'bitwise-ior #'bitwise-xor #'bitwise-not
              #'abs #'floor #'ceiling #'truncate #'round
              #'expt #'sqrt #'integer-sqrt #'log #'exp
              #'cos #'sin #'tan #'acos #'asin #'atan
              #'gcd #'lcm #'sgn #'sqr #'conjugate
              #'sinh #'cosh #'tanh
              #'fx+ #'fx- #'fx* #'fxquotient #'fxremainder #'fxmodulo #'fxabs
              #'unsafe-fx+ #'unsafe-fx- #'unsafe-fx* #'unsafe-fxquotient #'unsafe-fxremainder #'unsafe-fxmodulo #'unsafe-fxabs
              #'fxand #'fxior #'fxxor #'fxnot #'fxlshift #'fxrshift
              #'unsafe-fxand #'unsafe-fxior #'unsafe-fxxor #'unsafe-fxnot #'unsafe-fxlshift #'unsafe-fxrshift
              #'fxmax #'fxmin #'unsafe-fxmax #'unsafe-fxmin
              #'flabs #'fl+ #'fl- #'fl* #'fl/ #'flmin #'flmax
              #'unsafe-flabs #'unsafe-fl+ #'unsafe-fl- #'unsafe-fl* #'unsafe-fl/ #'unsafe-flmin #'unsafe-flmax
              #'flround #'flfloor #'flceiling #'fltruncate
              #'unsafe-flround #'unsafe-flfloor #'unsafe-flceiling #'unsafe-fltruncate
              #'flsin #'flcos #'fltan #'flasin #'flacos #'flatan
              #'unsafe-flsin #'unsafe-flcos #'unsafe-fltan #'unsafe-flasin #'unsafe-flacos #'unsafe-flatan
              #'fllog #'flexp #'flsqrt
              #'unsafe-fllog #'unsafe-flexp #'unsafe-flsqrt))))
