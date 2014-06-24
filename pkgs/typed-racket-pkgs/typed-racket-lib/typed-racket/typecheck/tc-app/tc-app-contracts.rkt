#lang racket/unit

;; This module provides custom type-checking rules for the expansion
;; of contracted values

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match
         racket/dict
         racket/format
         (env global-env)
         (typecheck signatures)
         (types base-abbrev resolve subtype union utils)
         (rep type-rep)
         (utils tc-utils)
         (for-template racket/base
                       ;; shift -1 because it's provided +1
                       racket/contract/private/provide))

(import tc-expr^)
(export tc-app-contracts^)

(define-tc/app-syntax-class (tc/app-contracts expected)
  (pattern (ctc-id:id blame e ...)
    ;; check that this is an application from the contract system
    #:when (contract-rename-id-property #'ctc-id)
    (check-contract #'ctc-id #'(e ...) expected)))

;; Assume that the contracted thing is of the same type the type
;; environment assigned to the exported identifier. Note that this
;; is only sound if the contract is a chaperone contract, so don't
;; put things in the base type environment if they have impersonator
;; contracts installed.
(define (check-contract orig-value-id other-args expected)
  (tc-expr/check #`(#%plain-app
                    #,(contract-rename-id-property orig-value-id)
                    . #,other-args)
                 expected))

