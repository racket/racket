#lang info

(define collection "slatex")

(define scribblings '(("slatex-wrap.scrbl" () (tool-library))))

;; (define tools (list (list "slatex-lang.rkt")))
;; (define tool-names (list "SLaTeX Language"))

(define mzscheme-launcher-names '("SLaTeX" "PDF SLaTeX"))
(define mzscheme-launcher-libraries '("slatex-launcher.rkt"
                                      "pdf-slatex-launcher.rkt"))
(define deps '("base"
               "compatibility-lib"))
(define build-deps '("scribble-lib"))
