#;
(exn-pred exn:fail:syntax? #rx".*could not be converted to a predicate.*")

#lang typed/racket/base

(make-predicate (Number -> Number))
