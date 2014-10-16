#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("at-exp-lib"
                     "base"
                     "eli-tester"
                     "xrepl-lib"))
(define update-implies '("xrepl-lib"))

(define pkg-desc "tests for \"xrepl\"")

(define pkg-authors '(eli))
