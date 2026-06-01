#lang info

(define collection 'multi)

(define deps '(["base" #:version "8.13.0.6"]
               "scheme-lib"
               ["zo-lib" #:version "1.3"]))

(define implies '("zo-lib"))

(define pkg-desc "implementation (no documentation) part of \"compiler\"")

(define pkg-authors '(mflatt))

(define version "1.16")

(define license
  '(Apache-2.0 OR MIT))
