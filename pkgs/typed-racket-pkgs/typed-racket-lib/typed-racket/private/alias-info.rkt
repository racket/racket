#lang racket/base

(require syntax/id-table)

(provide alias-info)

;; alias-info : (Dict Id (U String 'flat 'impersonator 'chaperone))
;;
;; This table stores if a given Name type matches up to a flat, impersonator,
;; or chaperone contract. If the contract failed to generate, then the
;; failure reason (a string) is stored.
(define alias-info (make-free-id-table))
