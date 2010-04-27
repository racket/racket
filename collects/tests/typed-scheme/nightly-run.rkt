#lang scheme/base

(require scheme/runtime-path)
(define-runtime-path run "run.ss")
(if (eq? 'cgc (system-type 'gc))
  (printf "Running under CGC => skipping tests\n")
  (dynamic-require run #f))
