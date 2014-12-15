#lang info

(define collection 'multi)
;; Do not add new dependencies to this package.
;; Tests that need dependencies belong in the "racket-test" or the
;;   "racket-test-extra" packages.
(define deps '("base"
               ;; the tests use -min.0 etc
               "unstable-flonum-lib"
               ;; the tests use zo-structs
               "compiler-lib"
               ;; the tests use the scribble reader
               "at-exp-lib"
               ;; the below are all tested here
               "sandbox-lib"
               "serialize-cstruct-lib"
               "cext-lib"))

(define pkg-desc "Minimal core version of Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
