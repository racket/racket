#lang racket/load

(module m typed/racket/base
  (provide (struct-out container))
  (struct: container ([value : Any])))

(require 'm racket/base)
(container-value (container (hasheq 'foo "foo")))