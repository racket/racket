#lang racket/base

;; a `main' submodule:
(module main racket/base
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () (printf "This is 16.\n"))
    #:exists 'append))
