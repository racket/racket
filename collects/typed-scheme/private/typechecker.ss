#lang scheme/base

(require "unit-utils.ss"
         mzlib/trace
         (only-in mzlib/unit provide-signature-elements)
         "signatures.ss" "tc-toplevel.ss"         
         "tc-if-unit.ss" "tc-lambda-unit.ss" "tc-app-unit.ss"
         "tc-let-unit.ss"
         "tc-expr-unit.ss" "check-subforms-unit.ss")

(provide-signature-elements typechecker^ tc-expr^)

(define-values/link-units/infer
  tc-toplevel@ tc-if@ tc-lambda@ tc-app@ tc-let@ tc-expr@ check-subforms@)
