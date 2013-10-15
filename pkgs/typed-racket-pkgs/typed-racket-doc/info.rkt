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
                     "typed-racket-lib"
                     "racket-doc"))
(define deps '("base"))

(define pkg-desc "documentation part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))
