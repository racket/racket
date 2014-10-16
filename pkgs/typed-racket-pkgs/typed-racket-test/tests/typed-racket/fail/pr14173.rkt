#;
(exn-pred "use a higher-order value")
#lang racket

(module t typed/racket #:no-optimize
  (provide f g)

  (define f (ann (case-lambda [() (add1 "hello")] [(x) x]) (Number -> Number)))
  (define g (ann f Any)))

(require 't)
(f 1)
(g)