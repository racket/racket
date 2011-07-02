#lang racket/base
(require "main.rkt")

(with-output-to-file "stdout"
  #:exists 'append
  (lambda () (displayln "alt")))
