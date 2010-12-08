#lang scheme/base

(require syntax/parse unstable/syntax
         (for-template scheme/base scheme/unsafe/ops)
         "../utils/utils.rkt"
         (types type-table)
         (optimizer utils))

(provide struct-opt-expr)

(define-syntax-class struct-opt-expr
  #:commit
  ;; we can always optimize struct accessors and mutators
  ;; if they typecheck, they're safe
  (pattern (#%plain-app op:id s:expr v:expr ...)
           #:when (or (struct-accessor? #'op) (struct-mutator? #'op))
           #:with opt
           (let ([idx (struct-fn-idx #'op)])
             (if (struct-accessor? #'op)
                 (begin (log-optimization "struct ref" #'op)
                        #`(unsafe-struct-ref  #,((optimize) #'s) #,idx))
                 (begin (log-optimization "struct set" #'op)
                        #`(unsafe-struct-set! #,((optimize) #'s) #,idx
                                              #,@(syntax-map (optimize) #'(v ...))))))))
