#lang info

(define collection 'multi)
(define deps '("base"
               "htdp-lib"))
(define build-deps '("scheme-lib"
                     "srfi-lite-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "racket-test"
                     "rackunit-lib"
                     "profile-lib"
                     "wxme-lib"))

(define pkg-desc "tests for \"htdp\"")

(define pkg-authors '(matthias mflatt robby))
