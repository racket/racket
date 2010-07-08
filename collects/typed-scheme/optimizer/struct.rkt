#lang scheme/base

(require syntax/parse
         syntax/id-table racket/dict
         unstable/match scheme/match
         (for-template scheme/base scheme/unsafe/ops)
         "../utils/utils.rkt"
         (rep type-rep)
         (types abbrev type-table utils subtype)
         (optimizer utils))

(provide struct-opt-expr)

(define-syntax-class struct-opt-expr
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
                                              #,@(map (optimize) (syntax->list #'(v ...)))))))))
