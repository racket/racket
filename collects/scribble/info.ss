#lang setup/infotab

(define mzscheme-launcher-names '("scribble"))
(define mzscheme-launcher-libraries '("run.ss"))
(define compile-omit-paths '("test-reader.ss"))

(define raco-commands
  '(("scribble" scribble/run "render a Scribble document" #f)))

