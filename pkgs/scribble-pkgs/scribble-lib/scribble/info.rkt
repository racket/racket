#lang info

(define source-keep-files '("doc"))

(define mzscheme-launcher-names '("scribble"))
(define mzscheme-launcher-libraries '("run.rkt"))

(define raco-commands
  '(("scribble" scribble/run "render a Scribble document" #f)))

(define purpose "This collect contains the implementation of scribble.")

(define release-note-files '(("Scribble" "HISTORY.txt")))

(define version "1.1")

(define test-responsibles '(("html.rkt" eli)))
