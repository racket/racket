#;
(exn-pred "Undefined")
#lang scheme/load

(module A scheme

  (define (f x) (add1 x))

  (provide f))

(module B typed/scheme

  (require/typed 'A [f (Integer -> Integer)])

  (let ()

    (: x Integer)
    (define x (f x))

    (void)))

(require 'B)
