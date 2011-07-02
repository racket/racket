#lang setup/infotab

(define mzscheme-launcher-names '("scribble"))
(define mzscheme-launcher-libraries '("run.rkt"))

(define raco-commands
  '(("scribble" scribble/run "render a Scribble document" #f)))

(define purpose "This collect contains the implementation of scribble.")
