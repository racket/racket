#lang info

(define collection 'multi)
(define deps '("typed-racket" "typed-racket-more" "typed-racket-compatibility"
               "unstable" "unstable-2d"))
(define build-deps '("racket-index"
                     "scheme-lib"
                     "base"
                     "racket-benchmarks"
                     "compatibility-lib"
                     "rackunit-lib"
                     "compiler-lib"
                     "redex-lib"
                     "htdp-lib"
                     "sandbox-lib"
                     "pconvert-lib"
                     "unstable-flonum-lib"
                     "unstable"))
(define update-implies '("typed-racket"))

(define pkg-desc "tests for \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.1")