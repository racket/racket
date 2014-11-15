#;(exn-pred #rx"wrong arity")
#lang typed/racket/base

(struct foo ([x : Integer] [y : Integer])
        #:guard (λ (x y) (values x y)))

(struct bar ([x : Integer] [y : Integer])
        #:guard (lambda: ([x : String] [y : String] [name : String])
                  (values x y)))

(struct baz ([x : Integer] [y : Integer])
        #:guard 3)
