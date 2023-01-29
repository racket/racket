#lang racket/base

;; Library for first-class components with recursive linking.

(require "private/unit/contract.rkt"
         "private/unit/keywords.rkt"
         "private/unit/runtime.rkt"
         "private/unit/signature.rkt"
         "private/unit/unit-core.rkt"
         "private/unit/unit-infer.rkt")

(provide only except rename import export prefix link tag init-depend extends contracted

         define-signature provide-signature-elements
         define-signature-form define-values-for-export open struct/ctc

         unit? unit define-unit define-unit-binding
         compound-unit define-compound-unit compound-unit/infer define-compound-unit/infer
         invoke-unit define-values/invoke-unit invoke-unit/infer define-values/invoke-unit/infer
         unit-from-context define-unit-from-context
         unit/new-import-export define-unit/new-import-export
         unit/s define-unit/s
         unit/c define-unit/contract)

(module+ compat
  ;; export only for compatibility with `mzlib/unit`
  (require (submod "private/unit/signature.rkt" compat))
  (provide (all-from-out (submod "private/unit/signature.rkt" compat))))
