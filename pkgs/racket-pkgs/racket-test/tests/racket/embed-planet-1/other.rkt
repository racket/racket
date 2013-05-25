#lang racket/base
(require (planet racket-tester/p2))

(with-output-to-file "stdout"
  #:exists 'append
  (lambda () (displayln "other")))
