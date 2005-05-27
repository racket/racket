
(module info (lib "infotab.ss" "setup")
  (define doc.txt "doc.txt")
  (define name "SLaTeX")

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
  (define mzscheme-launcher-flags
    (list (list "-qge" "(require (lib \"slatex-launcher.ss\" \"slatex\"))")
          (list "-qge" "(require (lib \"pdf-slatex-launcher.ss\" \"slatex\"))")))

  )
