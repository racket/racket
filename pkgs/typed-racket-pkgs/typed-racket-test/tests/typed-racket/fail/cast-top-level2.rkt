#;
(exn-pred exn:fail:syntax? #rx".*is unbound.*")

#lang racket/load

(require typed/racket/base)

(define: (a) (f (x : Number)) : a
  (cast x a))
