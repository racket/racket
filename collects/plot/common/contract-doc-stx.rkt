#lang racket/base

(require syntax/parse)

(provide (all-defined-out))

(struct proc+doc (proc-transformer doc-transformer)
    #:property prop:procedure (λ (p stx) ((proc+doc-proc-transformer p) stx)))
  
(define-syntax-class argument-spec
  #:description "argument specification"
  (pattern [name:id contract:expr])
  (pattern [name:id contract:expr default:expr])
  (pattern [kw:keyword name:id contract:expr])
  (pattern [kw:keyword name:id contract:expr default:expr]))