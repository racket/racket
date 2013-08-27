#lang info

(define collection "honu")
(define deps '("scheme-lib"
               "macro-debugger"
               "base"
               "parser-tools-lib"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "at-exp-lib"
                     "sandbox-lib"
                     "racket-doc"))

;; Make honu.vim easier to find by copying it to the "share" directory:
(define copy-shared-files '("contrib/honu.vim"))

(define pkg-desc "The implementation of the Honu language")

(define pkg-authors '(mflatt rafkind))
