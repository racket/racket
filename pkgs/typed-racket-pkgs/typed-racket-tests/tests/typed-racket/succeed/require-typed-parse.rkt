#lang racket/load

(module m1 racket
  (define x (make-parameter 1))
  (define y 1)
  (provide y)
  (provide/contract [x (parameter/c number?)]))

(module m2 typed/racket
  (require/typed 'm1
                 [y Number]
                 [x (Parameterof Number)])
  (x 1)
  (x))

(require 'm2)
