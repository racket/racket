#lang setup/infotab

(define collection "preprocessor")

(define mzscheme-launcher-names     '("mzpp"         "mztext"))
(define mzscheme-launcher-libraries '("mzpp-run.rkt" "mztext-run.rkt"))

(define scribblings '(("scribblings/preprocessor.scrbl" (multi-page) (legacy))))
(define deps '("base"
               "compatibility-lib"))
(define build-deps '("scribble-lib"))
