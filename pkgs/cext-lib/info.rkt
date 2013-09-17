#lang info
(define collection 'multi)
(define deps '("base"
               "compiler-lib"
               "scheme-lib"
               "rackunit-lib"))

(define pkg-desc "Tools for managing C extensions, such as `raco ctool`")

(define pkg-authors '(mflatt))
