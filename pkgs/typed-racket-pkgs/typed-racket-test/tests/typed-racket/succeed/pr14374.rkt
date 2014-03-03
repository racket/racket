#lang typed/racket

;; Test for PR 14374. Ensure that flvector is okay for Any

(require typed/rackunit
         racket/fixnum
         racket/flonum)

(check-equal? (flvector 0.0) (flvector 0.0))
(check-equal? (fxvector 0) (fxvector 0))

