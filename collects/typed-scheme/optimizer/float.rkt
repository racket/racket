#lang scheme/base

(require syntax/parse
         syntax/id-table racket/dict
         (for-template scheme/base scheme/flonum scheme/fixnum scheme/unsafe/ops)
         "../utils/utils.rkt"
         (types abbrev type-table utils subtype)
         (optimizer utils fixnum))

(provide (all-defined-out))

(define-syntax-class float-opt-expr
  (pattern e:expr
           #:when (subtypeof? #'e -Flonum)
           #:with opt ((optimize) #'e)))
(define-syntax-class int-opt-expr
  (pattern e:expr
           #:when (subtypeof? #'e -Integer)
           #:with opt ((optimize) #'e)))

;; if the result of an operation is of type float, its non float arguments
;; can be promoted, and we can use unsafe float operations
;; note: none of the unary operations have types where non-float arguments
;;  can result in float (as opposed to real) results
(define-syntax-class float-arg-expr
  (pattern e:fixnum-opt-expr
           #:with opt #'(unsafe-fx->fl e.opt))
  (pattern e:int-opt-expr
           #:with opt #'(->fl e.opt))
  (pattern e:float-opt-expr
           #:with opt #'e.opt))

(define (mk-float-tbl generic)
  (mk-unsafe-tbl generic "fl~a" "unsafe-fl~a"))

(define binary-float-ops 
  (mk-float-tbl (list #'+ #'- #'* #'/ #'min #'max)))
(define binary-float-comps
  (dict-set
   (dict-set
    (mk-float-tbl (list #'= #'<= #'< #'> #'>=))
    ;; not a comparison, but takes 2 floats and does not return a float,
    ;; unlike binary-float-ops
    #'make-rectangular #'unsafe-make-flrectangular)
   #'make-flrectangular #'unsafe-make-flrectangular))
(define unary-float-ops
  (mk-float-tbl (list #'abs #'sin #'cos #'tan #'asin #'acos #'atan #'log #'exp
                      #'sqrt #'round #'floor #'ceiling #'truncate)))

(define-syntax-class (float-op tbl)
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (dict-ref tbl #'i)))


(define (optimize-float-expr stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
                [e:float-opt-expr
                 (syntax/loc stx e.opt)]))
