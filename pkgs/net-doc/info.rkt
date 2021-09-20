#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "documentation part of \"net\"")

(define pkg-authors '(mflatt))
(define build-deps '("compatibility-lib"
                     "net-lib"
                     "racket-doc"
                     "scribble-lib"
                     "web-server-doc"
                     "web-server-lib"))
(define update-implies '("net-lib"))

(define version "1.1")

(define license
  '(Apache-2.0 OR MIT))
