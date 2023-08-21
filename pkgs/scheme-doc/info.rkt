#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               ["base" #:version "6.5.0.2"]
               "net-lib"
               "sandbox-lib"
               ["scribble-lib" #:version "1.34"]
               "racket-index"))
(define build-deps '("racket-doc"
                     "gui-lib"
                     "gui-doc"
                     "compatibility-doc"))

(define pkg-desc "Documentation for `scheme` languages and libraries")

(define pkg-authors '(mflatt samth))

(define version "1.3")

(define license
  '(Apache-2.0 OR MIT))
