#lang setup/infotab

(define scribblings '(("slatex-wrap.scrbl" () (tool-library))))

;; (define tools (list (list "slatex-lang.rkt")))
;; (define tool-names (list "SLaTeX Language"))

(define mzscheme-launcher-names '("SLaTeX" "PDF SLaTeX"))
(define mzscheme-launcher-libraries '("slatex-launcher.rkt"
                                      "pdf-slatex-launcher.rkt"))
