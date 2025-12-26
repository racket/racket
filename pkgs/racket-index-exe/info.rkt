#lang info

(define collection 'multi)

(define deps '("base"
               "scribble-lib"
               "racket-index"
               "scheme-lib"))
(define build-deps '())

(define pkg-desc "Racket Documentation executables")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))
