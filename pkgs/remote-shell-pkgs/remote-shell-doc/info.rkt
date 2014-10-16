#lang info

(define collection "remote-shell")

(define deps '("base"))
(define build-deps '("racket-doc"
                     "remote-shell-lib"
                     "scribble-lib"))
(define update-implies '("remote-shell-lib"))

(define pkg-desc "documentation part of \"remote-shell\"")

(define pkg-authors '(mflatt))


(define scribblings '(("remote-shell.scrbl" (multi-page))))
