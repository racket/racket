#lang racket/base
(require "embed-me13.rkt")
(with-output-to-file "stdout"
  (lambda () (printf "This is 14\n"))
  #:exists 'append)
