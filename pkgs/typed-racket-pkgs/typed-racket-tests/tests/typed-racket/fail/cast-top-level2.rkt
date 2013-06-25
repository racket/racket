#;
(exn-pred exn:fail:syntax? #rx".*Unbound type.*")

#lang racket/load

(require typed/racket/base)

(define: (a) (f (x : Number)) : a
  (cast x a))
