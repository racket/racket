#lang racket/base
(require (submod tests/compiler/embed/embed-me18a sub))
(with-output-to-file "stdout"
  (dynamic-require '(submod tests/compiler/embed/embed-me18a sub) 'print-18)
  #:exists 'append)
