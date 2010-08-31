#lang scheme/base

(require syntax/parse
         (for-template scheme/base scheme/flonum scheme/unsafe/ops)
         "../utils/utils.rkt"
         (optimizer utils))

(provide number-opt-expr)

(define-syntax-class number-opt-expr
  #:commit
  ;; these cases are all identity
  (pattern (#%plain-app (~and op (~or (~literal +) (~literal *) (~literal min) (~literal max)))
                        f:expr)
           #:with opt
           (begin (log-optimization "unary number" #'op)
                  ((optimize) #'f))))
