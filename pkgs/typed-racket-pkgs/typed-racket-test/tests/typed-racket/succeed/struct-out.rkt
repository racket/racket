#lang racket/load

(module untyped racket/base
 (struct posn (x y))
 (provide (struct-out posn)))

(module typed typed/racket/base
 (require-typed-struct posn ((x : Real) (y : Real)) 'untyped)
 (provide (struct-out posn)))

(require 'typed)
