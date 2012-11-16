#lang racket/base
(require scheme/unit
         scheme/contract)

(require "sig.rkt")

(require "embed-unit.rkt"
         "embed-sig.rkt")

(define-values/invoke-unit/infer compiler:embed@)

(provide/contract [make-embedding-executable
                   (->* (path-string?
                         any/c
                         any/c
                         (listof (or/c (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?))
                                       (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?)
                                               (listof symbol?))))
                         (listof path-string?)
                         any/c
                         (listof string?))
                        ((listof (cons/c symbol? any/c))
                         any/c
                         symbol?
                         (or/c #f
                               path-string?
                               (listof path-string?)))
                        void?)]
                  [create-embedding-executable
                   (->* (path-string?)
                        (#:modules 
                         (listof (or/c (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?))
                                       (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?)
                                               (listof symbol?))))
                         #:configure-via-first-module? any/c
                         #:literal-files (listof path-string?)
                         #:literal-expression any/c
                         #:literal-expressions (listof any/c)
                         #:cmdline (listof string?)
                         #:gracket? any/c
                         #:mred? any/c
                         #:variant (or/c '3m 'cgc)
                         #:aux (listof (cons/c symbol? any/c))
                         #:collects-path (or/c #f
                                               path-string?
                                               (listof path-string?))
                         #:collects-dest (or/c #f path-string?)
                         #:launcher? any/c
                         #:verbose? any/c
                         #:compiler (-> any/c compiled-expression?)
                         #:expand-namespace namespace?
                         #:src-filter (-> path? any)
                         #:on-extension (or/c #f (-> path-string? boolean? any))
                         #:get-extra-imports (-> path? compiled-module-expression? (listof module-path?)))
                        void?)])

(provide write-module-bundle
         embedding-executable-is-directory?
         embedding-executable-is-actually-directory?
         embedding-executable-put-file-extension+style+filters
         embedding-executable-add-suffix)
