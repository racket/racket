#lang info
(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               "errortrace-lib"
               "gui-lib"
               "pconvert-lib"
               "srfi-lib"
               "string-constants-lib"
               "trace"
               "wxme-lib"
               "drracket"
               "htdp"))

(define build-deps
  '("at-exp-lib"
    "racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define pkg-desc "Teaching languages for _Die Macht der Abstraktion_")

(define pkg-authors '(sperber))
