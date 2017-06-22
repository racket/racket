#lang info

(define collection 'multi)

(define deps '(["base" #:version "6.5.0.2"]
               "scheme-lib"
               "rackunit-lib"
               "zo-lib"))

(define implies '("zo-lib"))

(define pkg-desc "implementation (no documentation) part of \"compiler\"")

(define pkg-authors '(mflatt))

(define version "1.6")
