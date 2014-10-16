#lang info

(define collection "slatex")

(define scribblings '(("slatex-wrap.scrbl" () (tool-library))))

;; (define tools (list (list "slatex-lang.rkt")))
;; (define tool-names (list "SLaTeX Language"))

(define mzscheme-launcher-names '("SLaTeX" "PDF SLaTeX"))
(define mzscheme-launcher-libraries '("slatex-launcher.rkt"
                                      "pdf-slatex-launcher.rkt"))
(define deps '("scheme-lib"
               "base"
               "compatibility-lib"))
(define build-deps '("racket-index"
                     "eli-tester"
                     "racket-doc"
                     "scribble-lib"))

;; Make slatex.sty easier to find (for adding to TEXINPUTS)
;; by copying it to the "share" directory. Same for rubber support:
(define copy-shared-files '("slatex.sty" "slatex.py"))

(define pkg-desc "SLaTeX (Scheme in LaTeX)")

(define pkg-authors '(sstrickl))
