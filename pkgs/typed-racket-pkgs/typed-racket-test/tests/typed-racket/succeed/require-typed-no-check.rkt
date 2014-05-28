#lang typed/racket/no-check

;; This test checks require/typed options under no-check mode

(module untyped racket
  (define f values)
  (struct bar (x y))
  (struct baz (x y))
  (define-struct quux (x y))
  (provide f
           (struct-out bar)
           (struct-out baz)
           (struct-out quux)))

;; opaque is tested in pr14463.rkt
(require/typed 'untyped
               [f (-> String String)]
               [(f g) (-> String String)]
               [struct bar ([x : Integer] [y : Integer])]
               [#:struct baz ([x : Integer] [y : Integer])]
               [#:struct quux ([x : Integer] [y : Integer])
                #:constructor-name make-quux])

(f 3) (g 3)
(bar 1 2)
(make-quux 1 2)
