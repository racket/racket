#lang info

(define collection "distro-build")

(define deps '("base"
               "web-server-lib"
               "ds-store-lib"
               "net-lib"))
(define build-deps '("at-exp-lib"))

(define pkg-desc "Tools for constructing a distribution of Racket")

(define pkg-authors '(mflatt))
