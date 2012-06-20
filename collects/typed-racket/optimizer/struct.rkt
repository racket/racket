#lang racket/base

(require syntax/parse unstable/syntax
         (for-template racket/base racket/unsafe/ops)
         "../utils/utils.rkt"
         (utils tc-utils)
         (types type-table)
         (optimizer utils logging))

(provide struct-opt-expr)

(define struct-opt-msg "Struct access specialization.")

(define-syntax-class struct-opt-expr
  #:commit
  ;; we can always optimize struct accessors and mutators
  ;; if they typecheck, they're safe
  (pattern (#%plain-app op:id s:expr v:expr ...)
           #:when (or (struct-accessor? #'op) (struct-mutator? #'op))
           #:with opt
           (let ([idx (struct-fn-idx #'op)])
             (add-disappeared-use #'op)
             (if (struct-accessor? #'op)
                 (begin (log-optimization "struct ref" struct-opt-msg this-syntax)
                        #`(unsafe-struct-ref  #,((optimize) #'s) #,idx))
                 (begin (log-optimization "struct set" struct-opt-msg this-syntax)
                        #`(unsafe-struct-set! #,((optimize) #'s) #,idx
                                              #,@(syntax-map (optimize) #'(v ...))))))))
