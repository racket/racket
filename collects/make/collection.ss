#lang mzscheme

(require mzlib/unit
         dynext/file-sig
         dynext/file
         compiler/sig
         compiler/compiler
         compiler/option)

(require "make-sig.ss"
         "make.ss"
         "collection-sig.ss"
         "collection-unit.ss")

(define-values/invoke-unit/infer make:collection@)

(provide-signature-elements make:collection^)
