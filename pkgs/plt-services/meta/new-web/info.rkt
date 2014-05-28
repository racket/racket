#lang info

(define collection "www-racket-lang-org")

(define deps '("base"
               "plt-web-lib"
               "at-exp-lib"
               "net-lib"
               "racket-index"
               "scribble-lib"
               "syntax-color-lib"))

(define pkg-desc "Sources for http://racket-lang.org")

(define pkg-authors '(eli mflatt samth))

(define test-omit-paths '("sync.rkt"))
