#;
(exn-pred "x:.*cannot use before initialization")
#lang scheme/load

;; This test used to fail due to a type error, but TR no longer
;; tracks undefined statically, so it is just a dynamic error.

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
