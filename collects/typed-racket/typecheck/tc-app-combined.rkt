#lang racket/base

(require "../utils/utils.rkt"

         "tc-app/tc-app-apply.rkt"
         "tc-app/tc-app-eq.rkt"
         "tc-app/tc-app-hetero.rkt"
         "tc-app/tc-app-keywords.rkt"
         "tc-app/tc-app-lambda.rkt"
         "tc-app/tc-app-list.rkt"
         "tc-app/tc-app-objects.rkt"
         "tc-app/tc-app-special.rkt"
         "tc-app/tc-app-values.rkt"
         "tc-app/tc-app-main.rkt"
         "tc-app/signatures.rkt"
         "signatures.rkt")

(require racket/unit)
(provide tc-app-combined@)

(define-compound-unit/infer tc-app-combined@
  (import tc-expr^ tc-lambda^ tc-let^ tc-apply^)
  (export tc-app^)

  (link tc-app-main@
        tc-app-hetero@ tc-app-list@ tc-app-apply@ tc-app-values@ tc-app-keywords@
        tc-app-objects@ tc-app-eq@ tc-app-lambda@ tc-app-special@))
