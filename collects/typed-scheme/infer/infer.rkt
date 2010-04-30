#lang scheme/base

(require (except-in "../utils/utils.rkt" infer))
(require "infer-unit.rkt" "constraints.rkt" "dmap.rkt" "signatures.rkt"
         "restrict.rkt" "promote-demote.rkt"
         mzlib/trace
         (only-in scheme/unit provide-signature-elements
                  define-values/invoke-unit/infer link)
         (utils unit-utils))
         
(provide-signature-elements restrict^ infer^)

(define-values/invoke-unit/infer
  (link infer@ constraints@ dmap@ restrict@ promote-demote@))
