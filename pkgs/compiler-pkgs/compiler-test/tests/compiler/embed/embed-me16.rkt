#lang racket/base

;; a `main' submodule:
(module main racket/base
  (with-output-to-file "stdout"
    (lambda () (printf "This is 16.\n"))
    #:exists 'append))
