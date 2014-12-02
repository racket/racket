#lang racket/base


(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (displayln "two")))
