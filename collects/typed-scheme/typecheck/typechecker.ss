#lang scheme/base

(require "../utils/utils.ss")
(require (utils unit-utils)
         mzlib/trace
         (only-in scheme/unit provide-signature-elements)
         "signatures.ss" "tc-toplevel.ss"         
         "tc-if-unit.ss" "tc-lambda-unit.ss" "tc-app-unit.ss"
         "tc-let-unit.ss" "tc-dots-unit.ss"
         "tc-expr-unit.ss" "check-subforms-unit.ss")

(provide-signature-elements typechecker^ tc-expr^)

(define-values/link-units/infer
  tc-toplevel@ tc-if@ tc-lambda@ tc-dots@ tc-app@ tc-let@ tc-expr@ check-subforms@)
