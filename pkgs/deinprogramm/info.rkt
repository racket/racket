#lang info

(define collection "deinprogramm")

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

(define name "DeinProgramm")

(define tools '("deinprogramm-langs.rkt"))

(define tool-icons '(("logo-small.png" "deinprogramm")))
(define tool-names '("DeinProgramm"))
(define tool-urls '("http://www.deinprogramm.de/dmda/"))

(define compile-omit-files
  '("define-record-procedures.scm"
    "convert-explicit.scm"
    "line3d.scm"))

(define get-textbook-pls
  '("textbook-pls-spec.rkt" textbook-pls))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "at-exp-lib"
                     "scribble-lib"))

(define pkg-desc "Teaching languages for _Die Macht der Abstraktion_")

(define pkg-authors '(sperber))
