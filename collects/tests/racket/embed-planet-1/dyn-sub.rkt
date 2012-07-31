#lang racket/base
(require (submod (planet racket-tester/p1/has-sub) the-sub))

(with-output-to-file "stdout"
  #:exists 'append
  (lambda () (displayln (dynamic-require
                         '(submod (planet racket-tester/p1/has-sub) the-sub)
                         'out))))
