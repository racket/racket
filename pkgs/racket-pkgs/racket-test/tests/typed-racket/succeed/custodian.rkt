#lang typed/racket

;; Make sure the Custodian type is bound

(: cust Custodian)
(define cust (current-custodian))

