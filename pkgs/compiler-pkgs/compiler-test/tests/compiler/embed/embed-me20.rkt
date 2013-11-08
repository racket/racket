#lang racket/base

;; like "embed-me16.rkt" using `module+'
(module+ main
  (with-output-to-file "stdout"
    (lambda () (printf "This is 20.\n"))
    #:exists 'append))
