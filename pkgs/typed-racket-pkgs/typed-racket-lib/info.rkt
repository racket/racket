#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "srfi-lite-lib"
               "base"
               "pconvert-lib"
               "unstable-contract-lib"
               "unstable-list-lib"
               "unstable-debug-lib"
               "compatibility-lib" ;; to assign types
               "string-constants-lib"))


(define pkg-desc "implementation (no documentation) part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))
