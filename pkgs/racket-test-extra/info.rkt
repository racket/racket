#lang info

(define collection 'multi)

(define pkg-desc "Additional Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
(define build-deps '(["base" #:version "8.4"]
                     "drracket-tool-text-lib"
                     "redex-lib"
                     "scheme-lib"
                     "rackunit-lib"
                     "serialize-cstruct-lib"))

(define license
  '(Apache-2.0 OR MIT))
