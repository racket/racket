#lang scheme/base
(require scheme/contract)
(provide (all-from-out scheme/contract))

;; provide contracts for objects
(require scheme/private/contract-object)
(provide (all-from-out scheme/private/contract-object))
 


