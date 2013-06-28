#lang setup/infotab

;; This package holds infrastructure that is used to drive
;; builds and other services and that is not part of a
;; distribution.

(define collection 'multi)

(define deps '("distro-build"

               "at-exp-lib"
               "syntax-color-lib"
               "base"
               "gui-lib"
	       "net-lib"
               "sandbox-lib"
               "scribble-lib"
               "compatibility-lib"))
(define implies '("distro-build"))
