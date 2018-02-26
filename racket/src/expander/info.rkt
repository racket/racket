#lang info

(define collection "expander")

(define deps `(["base" #:version "6.6.0.2"]
               "zo-lib"
               "compiler-lib"))

(define build-deps `("at-exp-lib"))

(define pkg-desc "Racket's implementation of macros, modules, and top-level evaluation")

(define pkg-authors '(mflatt))
