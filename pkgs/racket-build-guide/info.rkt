#lang info

(define scribblings '(("racket-build-guide.scrbl" (multi-page) (racket-core -98))))

(define deps
  '("base"
    "scribble-lib"))

(define build-deps
  '("racket-doc"
    "scribble-doc"
    "distro-build-doc"))

(define pkg-desc "Racket build and contribution documentation")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
