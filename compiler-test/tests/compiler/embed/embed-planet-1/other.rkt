#lang racket/base
(require (planet racket-tester/p2))

(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  #:exists 'append
  (lambda () (displayln "other")))
