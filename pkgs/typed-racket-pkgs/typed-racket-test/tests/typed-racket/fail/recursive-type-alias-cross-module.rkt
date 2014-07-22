#;
(exn-pred #rx"expected a vector")
#lang racket/load

(module a typed/racket
  (define-type MyType (Vectorof (U Integer MyType)))
  (: x MyType)
  (define x (make-vector 3 3))
  (provide x))

(module b typed/racket
  (require 'a)
  (define y x)
  (provide y))

(require 'b)
(vector-set! y 0 "foo")
