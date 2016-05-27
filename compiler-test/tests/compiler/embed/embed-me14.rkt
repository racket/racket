#lang racket/base
(require "embed-me13.rkt")
(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (printf "This is 14\n"))
  #:exists 'append)
