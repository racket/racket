#lang scheme/base

(require syntax/parse
         syntax/id-table racket/dict
         (for-template scheme/base scheme/flonum scheme/unsafe/ops)
         "../utils/utils.rkt"
         (types abbrev type-table utils subtype)
         (optimizer utils fixnum))

(provide float-opt-expr)


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

(define-syntax-class float-expr
  (pattern e:expr
           #:when (subtypeof? #'e -Flonum)
           #:with opt ((optimize) #'e)))
(define-syntax-class int-expr
  (pattern e:expr
           #:when (subtypeof? #'e -Integer)
           #:with opt ((optimize) #'e)))

;; if the result of an operation is of type float, its non float arguments
;; can be promoted, and we can use unsafe float operations
;; note: none of the unary operations have types where non-float arguments
;;  can result in float (as opposed to real) results
(define-syntax-class float-arg-expr
  (pattern e:fixnum-expr
           #:with opt #'(unsafe-fx->fl e.opt))
  (pattern e:int-expr
           #:with opt #'(->fl e.opt))
  (pattern e:float-expr
           #:with opt #'e.opt))

(define-syntax-class float-opt-expr
  (pattern (~and res (#%plain-app (~var op (float-op unary-float-ops)) f:float-expr))
           #:when (subtypeof? #'res -Flonum)
           #:with opt
           (begin (log-optimization "unary float" #'op)
                  #'(op.unsafe f.opt)))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-ops))
                                  f1:float-arg-expr
                                  f2:float-arg-expr
                                  fs:float-arg-expr ...))
           ;; if the result is a float, we can coerce integers to floats and optimize
           #:when (subtypeof? #'res -Flonum)
           #:with opt
           (begin (log-optimization "binary float" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-comps))
                                  f1:float-expr
                                  f2:float-expr
                                  fs:float-expr ...))
           #:with opt
           (begin (log-optimization "binary float comp" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  
  ;; we can optimize exact->inexact if we know we're giving it an Integer
  (pattern (#%plain-app (~and op (~literal exact->inexact)) n:int-expr)
           #:with opt
           (begin (log-optimization "int to float" #'op)
                  #'(->fl n.opt)))
  ;; we can get rid of it altogether if we're giving it an inexact number
  (pattern (#%plain-app (~and op (~literal exact->inexact)) f:float-expr)
           #:with opt
           (begin (log-optimization "float to float" #'op)
                  #'f.opt)))
