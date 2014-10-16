#lang info

(define collection 'multi)

(define deps '("string-constants-lib" "base"))
(define build-deps '("racket-doc" "scribble-lib"))
(define pkg-desc "String constants documentation")
(define update-implies '("string-constants-lib"))

(define pkg-authors '(robby))
