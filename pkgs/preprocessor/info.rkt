#lang info

(define collection "preprocessor")

(define mzscheme-launcher-names     '("mzpp"         "mztext"))
(define mzscheme-launcher-libraries '("mzpp-run.rkt" "mztext-run.rkt"))

(define scribblings '(("scribblings/preprocessor.scrbl" (multi-page) (legacy))))
(define deps '("scheme-lib"
               "base"
               "compatibility-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

(define pkg-desc "Preprocessors for text with embedded Racket code (mostly replaced by scribble/text)")

(define pkg-authors '(eli))
