#lang typed/racket

;; A test for PR 14364. Should not infinite loop.

(struct: (X) Foo ([y : (U X (Pair (Foo X) (Foo X)))]))
(: x (Foo String))
(define x (Foo "a"))
