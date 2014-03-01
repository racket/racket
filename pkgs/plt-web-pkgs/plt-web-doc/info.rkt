#lang info

(define collection "plt-web")

(define pkg-desc "documentation part of \"plt-web\"")

(define pkg-authors '(eli mflatt samth))

(define deps '("base"
               "plt-web-lib"
               "racket-doc"
               "scribble-doc"
               "scribble-lib"))

(define scribblings '(("plt-web.scrbl")))
