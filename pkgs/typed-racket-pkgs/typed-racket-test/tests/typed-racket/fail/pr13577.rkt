#;
(exn-pred #rx"expected: Integer")

#lang typed/racket

(for/sum ([k (in-naturals)])
  (/ 1 (* k k)))

