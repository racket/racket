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
(define build-deps '("eli-tester"
                     "racket-doc"
                     "scribble-lib"))

;; Make slatex.sty easier to find (for adding to TEXINPUTS)
;; by copying it to the "lib" directory. Same for rubber support:
(define copy-foreign-libs '("slatex.sty" "slatex.py"))
