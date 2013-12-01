#lang info

;; This package references infrastructure that is used to drive
;; builds and other services and that is not part of a
;; distribution.

(define collection 'multi)

(define deps '("racket-index"
               ;; "Fake" dependencies to make them included
               ;; in a default build:
               "main-distribution-test"
               "distro-build"
               "honu"
               "gui-pkg-manager"
               
               ;; Actual dependencies:
               "eli-tester"
               "at-exp-lib"
               "syntax-color-lib"
               "base"
               "gui-lib"
	       "net-lib"
               "sandbox-lib"
               "scheme-lib"
               "scribble-lib"
               "compatibility-lib"))

(define pkg-desc "Miscellaneous management and maintenance tools used by the Racket development team")

(define pkg-authors '(eli mflatt))
