#lang info

(define collection "math")

(define scribblings '(["scribblings/math.scrbl" (multi-page)]))

(define compile-omit-paths '("tests"))
(define deps '("r6rs-lib"
               "base"
               "typed-racket-lib"
               "typed-racket-more"
               ("math-i386-macosx" #:platform "i386-macosx")
               ("math-x86_64-macosx" #:platform "x86_64-macosx")
               ("math-ppc-macosx" #:platform "ppc-macosx")
               ("math-win32-i386" #:platform "win32\\i386")
               ("math-win32-x86_64" #:platform "win32\\x86_64")))
(define build-deps '("racket-doc"
                     "typed-racket-doc"
                     "at-exp-lib"
                     "gui-lib"
                     "plot"
                     "sandbox-lib"
                     "scribble-lib"
                     "unstable-2d"))

(define pkg-desc "Functions and data structures useful for working with numbers and collections of numbers")

(define pkg-authors '(ntoronto))
