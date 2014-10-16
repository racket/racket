#lang info

(define collection 'multi)

(define build-deps '("net-doc"
                     "scheme-lib"
                     "srfi-lite-lib"
                     "compatibility-doc"
                     "r6rs-doc"
                     "srfi-doc"
                     "compatibility-lib"
                     "r6rs-lib"
                     "sandbox-lib"
                     "at-exp-lib"
                     "scribble-lib"
                     "pict-lib"
                     "typed-racket-lib"
                     "typed-racket-compatibility"
                     "typed-racket-more"
                     "racket-doc"))
(define deps '("base"))
(define update-implies '("typed-racket-lib"))

(define pkg-desc "documentation part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.1")