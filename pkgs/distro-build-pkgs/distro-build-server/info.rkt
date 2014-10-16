#lang info

(define collection "distro-build")

(define deps '("base"
               "distro-build-client"
               "web-server-lib"
               "ds-store-lib"
               "net-lib"
               "scribble-html-lib"
               "plt-web-lib"
               "remote-shell-lib"))
(define build-deps '("at-exp-lib"))

(define pkg-desc "server-side part of \"distro-build\"")

(define pkg-authors '(mflatt))
