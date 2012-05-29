#lang racket/base
(require rackunit
         "12658-mod.rkt")

(check-equal?
 (with-handlers ([exn? (λ (x) (exn-message x))])
   (go 42)
   (error 'go "Failed to throw exn"))
 "application: wrong number of arguments\n  procedure: go\n  expected number of arguments: 0\n  given number of arguments: 1\n  arguments:\n   42")


