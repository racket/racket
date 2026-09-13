#lang info

(define collection 'multi)

(define deps '(["base" #:version "9.2"]
               "ffi2-lib"
               "dynext-lib"
               "rackunit-lib"
               "at-exp-lib"
               "racket-doc"
               "scribble-lib"))
(define implies '("ffi2-lib"))

(define pkg-desc "Alternative to `ffi/unsafe` that's better tuned to modern Racket")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
