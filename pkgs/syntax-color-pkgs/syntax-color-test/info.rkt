#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"syntax-color-lib\"")

(define pkg-authors '(mflatt))
(define build-deps '("rackunit-lib"
                     "scheme-lib"
                     "syntax-color-lib"))
