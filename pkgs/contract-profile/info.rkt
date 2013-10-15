#lang info

(define collection "contract-profile")
(define deps '("base"
               "profile-lib"
               "unstable-list-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "rackunit-lib"
                     "math-lib"))

(define pkg-desc "Profiling tool for contracts")

(define pkg-authors '(stamourv))
