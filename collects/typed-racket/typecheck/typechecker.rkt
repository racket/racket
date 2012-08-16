#lang racket/base

(require "../utils/utils.rkt"
         racket/trace
         (only-in racket/unit
                  provide-signature-elements
                  define-values/invoke-unit/infer link)
         "signatures.rkt"
         "tc-app/tc-app-apply.rkt"
         "tc-app/tc-app-hetero.rkt"
         "tc-app/tc-app-keywords.rkt"
         "tc-app/tc-app-list.rkt"
         "tc-app/tc-app-values.rkt"
         "signatures.rkt"
         "tc-if.rkt" "tc-lambda-unit.rkt" "tc-app.rkt"
         "tc-let-unit.rkt" "tc-apply.rkt"
         "tc-expr-unit.rkt" "check-subforms-unit.rkt")

(provide-signature-elements tc-expr^ check-subforms^)

(define-values/invoke-unit/infer
  (link tc-if@ tc-lambda@ tc-app@ tc-let@ tc-expr@ check-subforms@ tc-apply@
        tc-app-hetero@ tc-app-list@ tc-app-apply@ tc-app-values@ tc-app-keywords@))
