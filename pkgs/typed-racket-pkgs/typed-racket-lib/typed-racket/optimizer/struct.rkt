#lang racket/base

(require syntax/parse syntax/stx
         (for-template racket/base racket/unsafe/ops)
         "../utils/utils.rkt"
         (utils tc-utils)
         (types type-table struct-table)
         (optimizer utils logging))

(provide struct-opt-expr)

(define struct-opt-msg "Struct access specialization.")

(define-syntax-class struct-op
  #:attributes (message opt idx)
  (pattern op:id
    #:when (struct-accessor? #'op)
    #:attr message "struct ref"
    #:with idx #`'#,(struct-fn-idx #'op)
    #:with opt #'unsafe-struct-ref)
  (pattern op:id
    #:when (struct-mutator? #'op)
    #:attr message "struct set"
    #:with idx #`'#,(struct-fn-idx #'op)
    #:with opt #'unsafe-struct-set!))

(define-syntax-class struct-opt-expr
  #:commit
  ;; we can always optimize struct accessors and mutators
  ;; if they typecheck, they're safe
  (pattern (#%plain-app op:struct-op s:opt-expr v:opt-expr ...)
    #:do [(add-disappeared-use #'op)
          (log-opt (attribute op.message) struct-opt-msg)]
    #:with opt #'(op.opt s.opt op.idx v.opt ...)))
