#lang racket/base

(require syntax/parse racket/extflonum
         syntax/parse/experimental/specialize
         (for-template racket/base racket/extflonum racket/unsafe/ops)
         "../utils/utils.rkt"
         (optimizer utils numeric-utils logging float))

(provide extflonum-opt-expr)


(define (mk-extflonum-tbl generic) ; all operations are prefixed
  (mk-unsafe-tbl generic "~a" "unsafe-~a"))

(define binary-extflonum-ops
  ;; comparisons are good too, because we don't have to do n-ary->binary
  ;; (all ops are binary only)
  (mk-extflonum-tbl (list #'extfl+ #'extfl- #'extfl* #'extfl/
                          #'extfl= #'extfl< #'extfl> #'extfl<= #'extfl>=
                          #'extflmin #'extflmax #'extflexpt)))

(define unary-extflonum-ops
  (mk-extflonum-tbl (list #'extflabs #'extflround #'extflfloor #'extflceiling #'extfltruncate
                          #'extflsin #'extflcos #'extfltan #'extflasin #'extflacos #'extflatan
                          #'extfllog #'extflexp #'extflsqrt)))

(define-syntax-class/specialize unary-extflonum-op (float-op unary-extflonum-ops))
(define-syntax-class/specialize binary-extflonum-op (float-op binary-extflonum-ops))


;; TODO do conversions (unsafe-fx->extfl, unsafe-extfl->fx)
;; TODO do extflvector ops

(define-syntax-rule (log-extfl-opt opt-label)
  (log-opt opt-label "Extflonum arithmetic specialization."))

;; Those are easy. Because extflonums are disjoint from the rest of the numeric
;; tower, if the operations type check at all, they have the right types for
;; optimization. Ops are all fixed arity, too.
(define-syntax-class extflonum-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app op:unary-extflonum-op t:opt-expr)
    #:do [(log-extfl-opt "unary extflonum")]
    #:with opt #'(op.unsafe t.opt))
  (pattern (#%plain-app op:binary-extflonum-op t1:opt-expr t2:opt-expr)
    #:do [(log-extfl-opt "binary extflonum")]
    #:with opt #'(op.unsafe t1.opt t2.opt)))
