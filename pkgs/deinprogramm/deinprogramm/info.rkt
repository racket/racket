#lang info

(define name "DeinProgramm")

(define tools '("deinprogramm-langs.rkt"))

(define tool-icons '(("logo-small.png" "deinprogramm")))
(define tool-names '("DeinProgramm"))
(define tool-urls '("http://www.deinprogramm.de/dmda/"))

(define compile-omit-paths
  '("define-record-procedures.scm"
    "convert-explicit.scm"
    "line3d.scm"))

(define get-textbook-pls
  '("textbook-pls-spec.rkt" textbook-pls))
