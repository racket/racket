#lang racket/base
(require "../private/sc.rkt"
         "../private/lib.rkt"
         "provide.rkt"
         unstable/wrapc
         (only-in syntax/parse/private/residual ;; keep abs. path
                  this-context-syntax)
         racket/contract/base)

(define-syntax-class (expr/c ctc-stx
                             #:positive [pos-blame 'use-site]
                             #:negative [neg-blame 'from-macro]
                             #:macro [macro-name #f]
                             #:name [expr-name #f]
                             #:context [ctx #f])
  #:attributes (c)
  (pattern y:expr
           #:with
           c (wrap-expr/c ctc-stx
                          #'y
                          #:positive pos-blame
                          #:negative neg-blame
                          #:name expr-name
                          #:macro macro-name
                          #:context (or ctx (this-context-syntax)))))

(provide-syntax-class/contract
 [expr/c (syntax-class/c (syntax?)
                         (#:positive (or/c syntax? string? module-path-index?
                                           'from-macro 'use-site 'unknown)
                          #:negative (or/c syntax? string? module-path-index?
                                           'from-macro 'use-site 'unknown)
                          #:name (or/c identifier? string? symbol? #f)
                          #:macro (or/c identifier? string? symbol? #f)
                          #:context (or/c syntax? #f)))])
