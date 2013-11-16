#lang racket/base

(require 
  (for-syntax syntax/parse racket/base
              "renamer.rkt"
              "../utils/tc-utils.rkt"))
(provide def-export)

(define-syntax (def-export stx)
  (syntax-parse stx
    [(def-export export-id:identifier id:identifier cnt-id:identifier)
     #'(define-syntax export-id
         (let ([c #'cnt-id])
           (if (unbox typed-context?)
               (renamer #'id c)
               (renamer c))))]))
