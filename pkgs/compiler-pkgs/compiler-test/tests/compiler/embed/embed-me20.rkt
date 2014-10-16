#lang racket/base

;; like "embed-me16.rkt" using `module+'
(module+ main
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () (printf "This is 20.\n"))
    #:exists 'append))
