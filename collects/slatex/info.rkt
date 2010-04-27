#lang setup/infotab

(define scribblings '(("slatex-wrap.scrbl" () (tool-library))))

;; (define tools (list (list "slatex-lang.ss")))
;; (define tool-names (list "SLaTeX Language"))

(define mzscheme-launcher-names '("SLaTeX" "PDF SLaTeX"))
(define mzscheme-launcher-libraries '("slatex-launcher.ss"
                                      "pdf-slatex-launcher.ss"))
