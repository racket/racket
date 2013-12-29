#lang racket/base

(module test racket/base
  (displayln "run as program for tests"))

(require racket/runtime-path)
(define-runtime-path run "run.rkt")
(parameterize ([current-command-line-arguments '#("--nightly")])
  (dynamic-require run #f))
