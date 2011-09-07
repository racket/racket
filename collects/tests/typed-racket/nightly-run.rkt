#lang racket/base

(require racket/runtime-path)
(define-runtime-path run "run.rkt")
(parameterize ([current-command-line-arguments '#("--nightly")])
  (dynamic-require run #f))
