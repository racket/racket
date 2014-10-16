#lang racket/base
(require "main.rkt")

(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  #:exists 'append
  (lambda () (displayln "alt")))
