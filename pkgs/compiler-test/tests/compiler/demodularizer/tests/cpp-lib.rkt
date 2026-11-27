#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)
(require (for-syntax racket/base))

;; A cross-phase persistent library: demodularizing with `-s` should
;; produce a cross-phase persistent module

(provide greet
         greeting)

(define-syntax (greeting stx)
  #'"hello")

(define (greet who)
  (string-append greeting ", " who))

(displayln (greet "world"))
