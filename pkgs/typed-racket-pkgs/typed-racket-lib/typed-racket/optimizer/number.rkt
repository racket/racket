#lang racket/base

(require syntax/parse
         (for-template racket/base)
         "../utils/utils.rkt"
         (utils tc-utils)
         (optimizer utils logging))

(provide number-opt-expr)

(define-literal-syntax-class unary-op (+ * min max))

(define-syntax-class number-opt-expr
  #:commit
  ;; these cases are all identity
  (pattern (#%plain-app op:unary-op f:opt-expr)
    #:do [(log-opt "unary number" "Identity elimination.")]
    #:with opt #'f.opt))
