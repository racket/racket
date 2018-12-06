#lang racket/base
(require "private/collapsible-common.rkt"
         (submod "private/collapsible-common.rkt" properties))

(provide 
 ;; collapsible functions and structures
 (struct-out collapsible-ho/c)
 (struct-out collapsible-leaf/c)
 (struct-out collapsible-property)
 (struct-out collapsible-count-property)
 (struct-out collapsible-wrapper-property)
 build-collapsible-leaf
 prop:collapsible-contract
 collapsible-contract-property?
 build-collapsible-contract-property
 collapsible-contract?
 merge
 collapsible-guard
 impersonator-prop:collapsible
 has-impersonator-prop:collapsible?
 get-impersonator-prop:collapsible)