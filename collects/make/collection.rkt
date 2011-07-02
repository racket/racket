#lang mzscheme

(require mzlib/unit
         dynext/file-sig
         dynext/file
         compiler/sig
         compiler/compiler
         compiler/option)

(require "make-sig.rkt"
         "make.rkt"
         "collection-sig.rkt"
         "collection-unit.rkt")

(define-values/invoke-unit/infer make:collection@)

(provide-signature-elements make:collection^)
