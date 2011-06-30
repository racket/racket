#lang racket/load

(module a racket
 (define-struct foo (bar baz))
 (define f (lambda (x) (+ (foo-bar x) 3)))

 (provide [struct-out foo]
          f))

(module b typed/racket
 (require/typed 'a
                [struct foo ([bar : Number] [baz : String])]
                [f (foo -> Number)])

 (f (foo 3 "4")))

(require 'b)
