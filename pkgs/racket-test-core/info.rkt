#lang info

(define collection 'multi)
;; Do not add new dependencies to this package.
;; Tests that need dependencies belong in the "racket-test" or the
;;   "racket-test-extra" packages.
(define deps '("base"
               ;; the tests use zo-structs
               "zo-lib"
               ;; the tests use the scribble reader
               "at-exp-lib"
               ;; the ffi tests use these
               "serialize-cstruct-lib"
               "dynext-lib"
               ;; sandboxes are tested here
               "sandbox-lib"))

(define pkg-desc "Minimal core version of Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
