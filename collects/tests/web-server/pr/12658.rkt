#lang racket/base
(require rackunit
         "12658-mod.rkt")

(check-equal?
 (with-handlers ([exn? (λ (x) (exn-message x))])
   (go 42)
   (error 'go "Failed to throw exn"))
 "procedure go: expects no arguments, given 1: 42")


