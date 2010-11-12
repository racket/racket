#lang scheme

(require syntax/parse
         "../utils/utils.rkt"
         (for-template scheme/base scheme/fixnum scheme/unsafe/ops)
         (types abbrev)
         (optimizer utils))

(provide fixnum-expr fixnum-opt-expr)


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
  #:commit
  (pattern (~or (~literal bitwise-not) (~literal fxnot)) #:with unsafe #'unsafe-fxnot)
  (pattern (~or (~literal abs)         (~literal fxabs)) #:with unsafe #'unsafe-fxabs))
;; closed on fixnums, but 2nd argument must not be 0
(define-syntax-class nonzero-fixnum-binary-op
  #:commit
  ;; quotient is not closed. (quotient most-negative-fixnum -1) is not a fixnum
  (pattern (~or (~literal modulo)    (~literal fxmodulo))    #:with unsafe #'unsafe-fxmodulo)
  (pattern (~or (~literal remainder) (~literal fxremainder)) #:with unsafe #'unsafe-fxremainder))

(define-syntax-class (fixnum-op tbl)
  #:commit
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (dict-ref tbl #'i)))


(define-syntax-class fixnum-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -Fixnum)
           #:with opt ((optimize) #'e)))
(define-syntax-class nonzero-fixnum-expr
  #:commit
  (pattern e:expr
           #:when (or (isoftype? #'e -PositiveFixnum) (isoftype? #'e -NegativeFixnum))
           #:with opt ((optimize) #'e)))

(define-syntax-class fixnum-opt-expr
  #:commit
  (pattern (#%plain-app op:fixnum-unary-op n:fixnum-expr)
           #:with opt
           (begin (log-optimization "unary fixnum" #'op)
                  #'(op.unsafe n.opt)))
  (pattern (#%plain-app (~var op (fixnum-op binary-fixnum-ops))
                        n1:fixnum-expr
                        n2:fixnum-expr
                        ns:fixnum-expr ...)
           #:with opt
           (begin (log-optimization "binary fixnum" #'op)
                  (n-ary->binary #'op.unsafe #'n1.opt #'n2.opt #'(ns.opt ...))))
  (pattern (#%plain-app op:nonzero-fixnum-binary-op
                        n1:fixnum-expr
                        n2:nonzero-fixnum-expr)
           #:with opt
           (begin (log-optimization "binary nonzero fixnum" #'op)
                  #'(op.unsafe n1.opt n2.opt)))

  (pattern (#%plain-app (~and op (~literal -)) f:fixnum-expr)
           #:with opt
           (begin (log-optimization "unary fixnum" #'op)
                  #'(unsafe-fx- 0 f.opt)))

  (pattern (#%plain-app (~and op (~literal exact->inexact)) n:fixnum-expr)
           #:with opt
           (begin (log-optimization "fixnum to float" #'op)
                  #'(unsafe-fx->fl n.opt)))

  (pattern (#%plain-app (~and op (~literal zero?)) n:fixnum-expr)
           #:with opt
           (begin (log-optimization "fixnum zero?" #'op)
                  #'(unsafe-fx= n.opt 0))))
