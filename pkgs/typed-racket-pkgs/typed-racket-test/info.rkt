#lang info

(define collection 'multi)
(define deps '("typed-racket" "typed-racket-more" "unstable"))
(define build-deps '("scheme-lib"
                     "base"
                     "compatibility-lib"
                     "rackunit-lib"
                     "compiler-lib"
                     "redex-lib"
                     "sandbox-lib"
                     "unstable-flonum-lib"
                     "unstable"))

(define pkg-desc "tests for \"typed-racket\"")

(define pkg-authors '(samth stamourv))
