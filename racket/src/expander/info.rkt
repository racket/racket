#lang info

(define collection "expander")

(define deps `(["base" #:version "8.4.0.2"]
               "zo-lib"
               "compiler-lib"))

(define build-deps `("at-exp-lib"))

(define pkg-desc "Racket's implementation of macros, modules, and top-level evaluation")

(define pkg-authors '(mflatt))

(define license '(Apache-2.0 OR MIT))
