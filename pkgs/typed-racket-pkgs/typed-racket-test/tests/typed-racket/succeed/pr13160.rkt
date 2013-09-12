#lang racket/load

(module a typed/racket/base
  (provide (struct-out the-struct))
  (struct: the-struct ((a : Number))))


(module b typed/racket/base
  (require 'a)
  (provide (struct-out the-struct)))

(require 'b)
(the-struct 5)

