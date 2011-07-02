#lang scheme/base

(require scheme/runtime-path)
(define-runtime-path run "run.rkt")
(if (eq? 'cgc (system-type 'gc))
  (printf "Running under CGC => skipping tests\n")
  (parameterize ([current-command-line-arguments '#("--nightly")])
    (dynamic-require run #f)))
