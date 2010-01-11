#lang setup/infotab

(define name "DeinProgramm")

(define tools '("deinprogramm-langs.ss"))

(define tool-icons '(("logo-small.png" "deinprogramm")))
(define tool-names '("DeinProgramm"))
(define tool-urls '("http://www.deinprogramm.de/dmda/"))

(define compile-omit-files
  '("define-record-procedures.scm"
    "convert-explicit.scm"
    "line3d.scm"))
