#lang racket/base

(require "../../utils/utils.rkt"

         "tc-app-apply.rkt"
         "tc-app-eq.rkt"
         "tc-app-hetero.rkt"
         "tc-app-keywords.rkt"
         "tc-app-lambda.rkt"
         "tc-app-list.rkt"
         "tc-app-objects.rkt"
         "tc-app-special.rkt"
         "tc-app-values.rkt"
         "signatures.rkt"

         (typecheck signatures tc-app))

(require racket/unit)
(provide tc-app-combined@)

(define-compound-unit/infer tc-app-combined@
  (import tc-expr^ tc-lambda^ tc-let^ tc-apply^)
  (export tc-app^)

  (link tc-app@ 
        tc-app-hetero@ tc-app-list@ tc-app-apply@ tc-app-values@ tc-app-keywords@
        tc-app-objects@ tc-app-eq@ tc-app-lambda@ tc-app-special@))
