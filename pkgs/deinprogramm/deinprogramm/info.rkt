#lang setup/infotab
(require string-constants)

(define name "DeinProgramm")

(define tools '("deinprogramm-langs.rkt"))

(define tool-icons '(("logo-small.png" "deinprogramm")))
(define tool-names '("DeinProgramm"))
(define tool-urls '("http://www.deinprogramm.de/dmda/"))

(define compile-omit-files
  '("define-record-procedures.scm"
    "convert-explicit.scm"
    "line3d.scm"))

(define textbook-pls
  (list (list '("logo-small.png" "deinprogramm")
              "DeinProgramm"
              (string-constant teaching-languages)
              "DeinProgramm"
              "Die Macht der Abstraktion - Anfänger")))
