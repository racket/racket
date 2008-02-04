
(module info setup/infotab
  (define name "SLaTeX")

  (define scribblings '(("slatex-wrap.scrbl")))

  ;(define tools (list (list "slatex-lang.ss")))
  ;(define tool-names (list "SLaTeX Language"))

  (define mzscheme-launcher-names (list "SLaTeX" "PDF SLaTeX"))
  (define mzscheme-launcher-libraries
    (list "slatex-launcher.ss"
          "pdf-slatex-launcher.ss"))

  )
