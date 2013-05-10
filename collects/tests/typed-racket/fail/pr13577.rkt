#;
(exn-pred #rx"Expected result: AnyValues")

#lang typed/racket

(for/sum ([k (in-naturals)])
  (/ 1 (* k k)))

