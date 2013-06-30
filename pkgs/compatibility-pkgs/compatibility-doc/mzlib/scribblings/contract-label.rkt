#lang racket/base
(require (for-label racket/contract)
         scribble/manual)
(provide (all-defined-out))

;; this file establishes the right for-label
;; bindings so that I can link to racket/contract
;; combinators in the mzlib/contract docs

(define r:-> (racket ->))
(define r:->* (racket ->*))
(define r:->i (racket ->i))
(define r:->d (racket ->d))
