#lang racket/base
(require (submod "embed-me15-one.rkt" one))
(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (printf "This is ~a.\n" (+ 9 one two three)))
  #:exists 'append)
