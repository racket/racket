#lang racket/base
(require rackunit
         "12658-mod.rkt")

(check-equal?
 (with-handlers ([exn? (λ (x) (exn-message x))])
   (go 42)
   (error 'go "Failed to throw exn"))
 "go: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 0\n  given: 1\n  arguments...:\n   42")


