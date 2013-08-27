#lang info

(define collection "trace")

(define name "Calltrace")

(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"))

(define pkg-desc "Instrumentation to show function calls")

(define pkg-authors '(mflatt robby))
