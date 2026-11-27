#lang racket/base

(require scribble/manual
         (for-label racket/base
                    racket/contract))

(provide (all-from-out scribble/manual)
         (for-label (except-out (all-from-out racket/base
                                              racket/contract)
                                #%module-begin))
         refman
         cross-phase-persistent-lib)

(define refman '(lib "scribblings/reference/reference.scrbl"))

;; For use in a `defmodule` body: @cross-phase-persistent-lib[syntax/stx]
(define-syntax-rule (cross-phase-persistent-lib lib)
  (cross-phase-persistent-lib-note (racketmodname lib)))

(define (cross-phase-persistent-lib-note lib-elem)
  (list "The " lib-elem " library is a "
        (tech #:doc refman "cross-phase persistent")
        " module."))
