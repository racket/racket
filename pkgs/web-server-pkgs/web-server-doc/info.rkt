#lang info

(define collection 'multi)

(define build-deps '("net-doc"
                     "rackunit-doc"
                     "compatibility-doc"
                     "db-doc"
                     "scribble-doc"
                     "unstable-doc"
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
(define update-implies '("web-server-lib"))

(define pkg-desc "documentation part of \"web-server\"")

(define pkg-authors '(jay))
