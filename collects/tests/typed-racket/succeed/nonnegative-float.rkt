#lang typed/scheme

(ann (+ 1.0 2.1) Nonnegative-Float)
(ann (+ 1 2.1) Nonnegative-Float)
(ann (* 1.2 3.1) Nonnegative-Float)
(ann (sqrt 3.5) Nonnegative-Float)
