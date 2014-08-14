#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "documentation part of \"net\"")

(define pkg-authors '(mflatt))
(define build-deps '("compatibility-lib"
                     "net-lib"
                     "racket-doc"
                     "scribble-lib"
                     "unstable-contract-lib"
                     "unstable-doc"
                     "web-server-doc"
                     "web-server-lib"))
(define update-implies '("net-lib"))
