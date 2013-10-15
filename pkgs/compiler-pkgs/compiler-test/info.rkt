#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"compiler-lib\"")

(define pkg-authors '(mflatt))
(define build-deps '("compiler-lib"
                     "eli-tester"
                     "net-lib"
                     "scheme-lib"))
