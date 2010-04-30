#lang scheme/base

(require "../utils/utils.rkt")
(require (utils unit-utils)
         mzlib/trace
         (only-in scheme/unit
                  provide-signature-elements
                  define-values/invoke-unit/infer link)
         "signatures.rkt" "tc-toplevel.rkt"
         "tc-if.rkt" "tc-lambda-unit.rkt" "tc-app.rkt"
         "tc-let-unit.rkt" "tc-dots-unit.rkt"
         "tc-expr-unit.rkt" "check-subforms-unit.rkt")

(provide-signature-elements typechecker^ tc-expr^)

(define-values/invoke-unit/infer
  (link tc-toplevel@ tc-if@ tc-lambda@ tc-dots@ tc-app@ tc-let@ tc-expr@ check-subforms@))
