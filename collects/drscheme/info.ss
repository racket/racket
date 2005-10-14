(module info (lib "infotab.ss" "setup")
  (require (lib "string-constant.ss" "string-constants"))
  
  ;; added a comment, in windows ...
  (define name "DrScheme")
  (define tools (list "syncheck.ss"))
  (define tool-names (list "Check Syntax"))
  (define splash-questions
    (list (list (string-constant use-with-htdp)
                (string-constant teaching-languages)
                (string-constant how-to-design-programs)
                (string-constant beginning-student))
                
          (list (string-constant use-seasoned)
                (string-constant professional-languages)
                "(module ...)")
          (list (string-constant use-other)
                (string-constant professional-languages)
                (string-constant plt)
                (string-constant pretty-big-scheme))))
  (define mred-launcher-names     (list "DrScheme"))
  (define mred-launcher-libraries (list "drscheme.ss"))
  (define mred-launcher-flags     (list (list "-ZmvqL" "drscheme.ss" "drscheme"))))
