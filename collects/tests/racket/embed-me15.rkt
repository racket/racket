#lang racket/base
(require (submod "embed-me15-one.rkt" one))
(with-output-to-file "stdout"
  (lambda () (printf "This is ~a.\n" (+ 9 one two three)))
  #:exists 'append)
