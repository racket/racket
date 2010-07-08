#lang scheme

(require syntax/parse
         "../utils/utils.rkt"
         (for-template scheme/base scheme/fixnum scheme/unsafe/ops)
         (types abbrev type-table utils subtype)
         (optimizer utils))

(provide (all-defined-out))

(define-syntax-class fixnum-opt-expr
  (pattern e:expr
           #:when (subtypeof? #'e -Fixnum)
           #:with opt ((optimize) #'e)))
(define-syntax-class nonzero-fixnum-opt-expr
  (pattern e:expr
           #:when (or (isoftype? #'e -PositiveFixnum) (isoftype? #'e -NegativeFixnum))
           #:with opt ((optimize) #'e)))

(define (mk-fixnum-tbl generic)
  (mk-unsafe-tbl generic "fx~a" "unsafe-fx~a"))

;; due to undefined behavior when results are out of the fixnum range, only some
;; fixnum operations can be optimized
;; the following must be closed on fixnums
(define binary-fixnum-ops
  (dict-set
   (dict-set
    (dict-set
     (dict-set
      (dict-set
       (dict-set
        (mk-fixnum-tbl (list #'= #'<= #'< #'> #'>= #'min #'max))
        #'bitwise-and #'unsafe-fxand)
       #'fxand #'unsafe-fxand)
      #'bitwise-ior #'unsafe-fxior)
     #'fxior #'unsafe-fxior)
    #'bitwise-xor #'unsafe-fxxor)
   #'fxxor #'unsafe-fxxor))
(define-syntax-class fixnum-unary-op
  (pattern (~or (~literal bitwise-not) (~literal fxnot)) #:with unsafe #'unsafe-fxnot)
  (pattern (~or (~literal abs)         (~literal fxabs)) #:with unsafe #'unsafe-fxabs))
;; closed on fixnums, but 2nd argument must not be 0
(define-syntax-class nonzero-fixnum-binary-op
  (pattern (~or (~literal quotient)  (~literal fxquotient))  #:with unsafe #'unsafe-fxquotient)
  (pattern (~or (~literal modulo)    (~literal fxmodulo))    #:with unsafe #'unsafe-fxmodulo)
  (pattern (~or (~literal remainder) (~literal fxremainder)) #:with unsafe #'unsafe-fxremainder))

(define-syntax-class (fixnum-op tbl)
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (dict-ref tbl #'i)))


(define (optimize-finum-expr stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
                [e:fixnum-opt-expr
                 (syntax/loc stx e.opt)]))
