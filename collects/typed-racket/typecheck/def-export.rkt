#lang racket/base

(require racket/require (for-template "renamer.rkt") "renamer.rkt"
         (for-syntax syntax/parse racket/base "renamer.rkt"
                     (path-up "utils/tc-utils.rkt" "env/type-name-env.rkt")))
(provide def-export)

(define-syntax (def-export stx)
  (syntax-parse stx
    [(def-export export-id:identifier id:identifier cnt-id:identifier)
     #'(define-syntax export-id
         (if (unbox typed-context?)
             (renamer #'id #'cnt-id)
             (renamer #'cnt-id)))]))
