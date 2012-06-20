#lang racket/base

(require syntax/parse
         (for-template racket/base)
         "../utils/utils.rkt"
         (utils tc-utils)
         (optimizer utils logging))

(provide number-opt-expr)

(define-syntax-class number-opt-expr
  #:commit
  ;; these cases are all identity
  (pattern (#%plain-app (~and op (~or (~literal +) (~literal *)
                                      (~literal min) (~literal max)))
                        f:expr)
           #:with opt
           (begin (log-optimization "unary number" "Identity elimination."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  ((optimize) #'f))))
