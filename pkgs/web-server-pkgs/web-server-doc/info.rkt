#lang info

(define collection 'multi)

(define build-deps '("compatibility-doc"
                     "db-doc"
                     "scribble-doc"
                     "unstable"
                     "compatibility-lib"
                     "db-lib"
		     "net-lib"
                     "rackunit-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "unstable-contract-lib"
                     "web-server-lib"
                     "racket-doc"))
(define deps '("base"))

(define pkg-desc "documentation part of \"web-server\"")

(define pkg-authors '(jay))
