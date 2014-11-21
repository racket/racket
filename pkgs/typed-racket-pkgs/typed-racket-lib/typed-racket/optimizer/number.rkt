#lang racket/base

(require syntax/parse
         syntax/parse/experimental/specialize
         (for-template racket/base)
         "../utils/utils.rkt"
         (types numeric-tower union)
         (optimizer utils logging))

(provide number-opt-expr)

(define-literal-syntax-class unary-op (+ * min max))
(define-literal-syntax-class real-part)
(define-literal-syntax-class imag-part)
(define-literal-syntax-class abs)
(define-literal-syntax-class magnitude)
(define-literal-syntax-class angle)

(define-syntax-class/specialize real-expr
  (subtyped-expr -Real))
;; We don't want -0.0 here
(define-syntax-class/specialize non-neg-real-expr
  (subtyped-expr (Un -PosReal -Zero -InexactRealPosZero)))


(define-syntax-class number-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  ;; these cases are all identity
  (pattern (#%plain-app op:unary-op f:opt-expr)
    #:do [(log-opt "unary number" "Identity elimination.")]
    #:with opt #'f.opt)

  (pattern (#%plain-app op:real-part^ f:real-expr)
    #:do [(log-opt "unary number" "Projection elimination.")]
    #:with opt #'f.opt)

  (pattern (#%plain-app op:imag-part^ f:real-expr)
    #:do [(log-opt "unary number" "Projection elimination.")]
    #:with opt #'0)

  (pattern (#%plain-app (~or op:abs^ op:magnitude^) f:non-neg-real-expr)
    #:do [(log-opt "unary number" "Projection elimination.")]
    #:with opt #'f.opt)

  (pattern (#%plain-app op:angle^ f:non-neg-real-expr)
    #:do [(log-opt "unary number" "Projection elimination.")]
    #:with opt #'0))
