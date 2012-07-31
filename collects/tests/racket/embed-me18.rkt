#lang racket/base
(require (submod tests/racket/embed-me18a sub))
(with-output-to-file "stdout"
  (dynamic-require '(submod tests/racket/embed-me18a sub) 'print-18)
  #:exists 'append)
