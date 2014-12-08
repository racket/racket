#lang racket/base
(require
  rackunit
  setup/parallel-build)

(check-exn exn:fail:contract? (λ () (parallel-compile ".")))
(check-exn exn:fail:contract? (λ () (parallel-compile-files (list) #:worker-count 2.5)))
(check-exn exn:fail:contract? (λ () (parallel-compile-files (list) #:handler 5)))
