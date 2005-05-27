(module info (lib "infotab.ss" "setup")
  ;; added a comment, in windows ...
  (define name "DrScheme")
  (define tools (list "syncheck.ss"))
  (define tool-names (list "Check Syntax"))
  (define mred-launcher-names     (list "DrScheme"))
  (define mred-launcher-libraries (list "drscheme.ss"))
  (define mred-launcher-flags     (list (list "-ZmvqL" "drscheme.ss" "drscheme"))))
