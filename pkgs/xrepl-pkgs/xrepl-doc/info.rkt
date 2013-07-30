#lang info

(define collection 'multi)

(define build-deps '("errortrace-doc"
                     "macro-debugger"
                     "profile-doc"
                     "readline-doc"
                     "macro-debugger-text-lib"
                     "profile-lib"
                     "readline-lib"
                     "xrepl-lib"
                     "racket-doc"))
(define deps '("base"
               "sandbox-lib"
               "scribble-lib"))

(define pkg-desc "documentation part of \"xrepl\"")

(define pkg-authors '(eli))
