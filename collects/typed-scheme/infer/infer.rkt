#lang scheme/base

(require (except-in "../utils/utils.ss" infer))
(require "infer-unit.ss" "constraints.ss" "dmap.ss" "signatures.ss"
         "restrict.ss" "promote-demote.ss"
         mzlib/trace
         (only-in scheme/unit provide-signature-elements
                  define-values/invoke-unit/infer link)
         (utils unit-utils))
         
(provide-signature-elements restrict^ infer^)

(define-values/invoke-unit/infer
  (link infer@ constraints@ dmap@ restrict@ promote-demote@))
