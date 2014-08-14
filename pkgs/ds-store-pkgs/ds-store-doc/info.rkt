#lang info

(define collection "ds-store")
(define deps '("base"
               "scribble-lib"
               "racket-doc"
               "ds-store-lib"))
(define update-implies '("ds-store-lib"))

(define scribblings '(("ds-store.scrbl")))


(define pkg-desc "documentation part of \"ds-store\"")

(define pkg-authors '(mflatt))
