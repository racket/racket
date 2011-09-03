#;
(exn-pred 1)
#lang typed/scheme

(ann (- 1.0 0.5) Nonnegative-Float) ; can't prove it's nonnegative
