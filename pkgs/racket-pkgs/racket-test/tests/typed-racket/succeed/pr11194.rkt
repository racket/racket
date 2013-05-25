#lang racket/load

(module untyped racket
 (struct foo (bar baz))
 (define f (lambda (x) (+ (foo-baz x) 3)))

 (provide [struct-out foo]
          f))

(module typed typed/racket
 (require/typed 'untyped
                [struct foo ([bar : Number] [baz : String])]))

(require 'typed)
