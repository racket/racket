#lang info

(define collection 'multi)

(define deps '("base"))

(define build-deps '("data-doc"
                     "srfi-lite-lib"
                     "srfi-doc"
                     "web-server-doc"
                     "base"
                     "scribble-lib"
                     "sandbox-lib"
                     "web-server-lib"
                     "db-lib"
                     "racket-doc"))
(define update-implies '("db-lib"))

(define pkg-desc "documentation part of \"db\"")

(define pkg-authors '(ryanc))
