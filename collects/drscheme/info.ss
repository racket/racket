(module info setup/infotab
  (define name "DrScheme")
  (define tools (list "syncheck.ss" (list "time-keystrokes.ss" "private")))
  (define tool-names (list "Check Syntax" "Time Keystrokes"))
  (define mred-launcher-names     (list "DrScheme"))
  (define mred-launcher-libraries (list "drscheme.ss"))
  (define scribblings '(("tools.scrbl" (multi-page main-doc))
                        ("drscheme.scrbl" (#;multi-page main-doc)))))

