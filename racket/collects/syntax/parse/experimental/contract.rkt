#lang racket/base
(require syntax/parse/pre
         "provide.rkt"
         syntax/contract
         (only-in syntax/parse/private/residual ;; keep abs. path
                  this-context-syntax
                  this-role)
         racket/contract/base)

(define not-given (gensym))

(define-syntax-class (expr/c ctc-stx
                             #:arg? [arg? #t]
                             #:positive [pos-blame 'from-macro]
                             #:negative [neg-blame 'use-site]
                             #:macro [macro-name #f]
                             #:name [expr-name not-given]
                             #:context [ctx #f])
  #:attributes (c)
  #:commit
  (pattern y:expr
           #:with
           c (wrap-expr/c ctc-stx
                          #'y
                          #:arg? arg?
                          #:positive pos-blame
                          #:negative neg-blame
                          #:name (if (eq? expr-name not-given)
                                     this-role
                                     expr-name)
                          #:macro macro-name
                          #:context (or ctx (this-context-syntax)))))

(provide-syntax-class/contract
 [expr/c (syntax-class/c (syntax?)
                         (#:arg? any/c
                          #:positive (or/c syntax? string? module-path-index?
                                           'from-macro 'use-site 'unknown)
                          #:negative (or/c syntax? string? module-path-index?
                                           'from-macro 'use-site 'unknown)
                          #:name (or/c identifier? string? symbol? #f)
                          #:macro (or/c identifier? string? symbol? #f)
                          #:context (or/c syntax? #f)))])
