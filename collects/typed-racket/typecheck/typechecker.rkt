#lang racket/base

(require "../utils/utils.rkt"
         racket/trace
         (only-in racket/unit
                  provide-signature-elements
                  define-values/invoke-unit/infer link)
         "signatures.rkt"
         "tc-app-combined.rkt"
         "tc-if.rkt" "tc-lambda-unit.rkt"
         "tc-let-unit.rkt" "tc-apply.rkt"
         "tc-expr-unit.rkt" "check-subforms-unit.rkt")

(provide-signature-elements tc-expr^ check-subforms^)

(define-values/invoke-unit/infer
  (link tc-if@ tc-lambda@ tc-app-combined@ tc-let@ tc-expr@
        check-subforms@ tc-apply@))
