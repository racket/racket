#lang racket/base

(require syntax/parse
         (for-template racket/base)
         "../utils/utils.rkt"
         (utils tc-utils)
         (optimizer utils logging binary-expansion))

(provide number-opt-expr)

(define-literal-syntax-class unary-op (+ * min max))
(define-literal-syntax-class binary-op (+ * - /))

(define-syntax-class number-opt-expr
  #:commit
  #:attributes (opt)
  ;; these cases are all identity
  (pattern (#%plain-app op:unary-op f:opt-expr)
    #:do [(log-opt "unary number" "Identity elimination.")]
    #:with opt #'f.opt)

  ;; Turn multiway operators into a tree of binary operations
  (pattern (#%plain-app op:binary-op (~between args:expr 3 +inf.0) ...)
    #:with :opt-expr (binary-expand this-syntax)))
