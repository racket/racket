#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("racket-index"
                     "scheme-lib"
                     "draw-lib"
                     "racket-test"
                     "sgl"
                     "snip-lib"
                     "wxme-lib"
                     "gui-lib"
                     "rackunit-lib"
                     "pconvert-lib"
                     "compatibility-lib"
                     "sandbox-lib"))
(define update-implies '("gui-lib"))

(define pkg-desc "tests for \"gui\"")

(define pkg-authors '(mflatt robby))
