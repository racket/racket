
(module info setup/infotab
  (define name "SLaTeX")

  (define scribblings '(("slatex-wrap.scrbl")))

  ;(define tools (list (list "slatex-lang.ss")))
  ;(define tool-names (list "SLaTeX Language"))

  (define help-desk-message
    "Mz/Mr: (require (lib \"slatex-wrapper.ss\" \"slatex\"))")
  (define blurb
    (list "SLaTeX is a pre-processor for LaTeX that formats Scheme code. "
	  "For more information, see "
	  `(tt () "slatxdoc.dvi")
	  " in the "
	  `(tt () ,(path->string (build-path (collection-path "slatex") "slatex-code")))
	  " directory on this machine."))

  (define mzscheme-launcher-names (list "SLaTeX" "PDF SLaTeX"))
  (define mzscheme-launcher-libraries
    (list "slatex-launcher.ss"
          "pdf-slatex-launcher.ss"))

  )
