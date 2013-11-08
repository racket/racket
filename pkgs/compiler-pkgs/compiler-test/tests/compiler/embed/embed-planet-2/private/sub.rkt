#lang racket/base
(require "../main.ss")

(with-output-to-file "stdout"
  #:exists 'append
  (lambda () (displayln "sub")))
