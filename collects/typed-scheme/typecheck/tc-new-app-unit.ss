#lang scheme/unit

(require "signatures.ss" "../utils/utils.ss")
(require (utils tc-utils))

(import tc-expr^ tc-lambda^ tc-dots^)
(export tc-app^)

(define (tc/app . args)
  (int-err "tc/app NYI"))

(define (tc/app/check . args)
  (int-err "tc/app/check NYI"))

(define (tc/funapp . args)
  (int-err "tc/funapp NYI"))


