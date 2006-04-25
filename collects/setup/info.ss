(module info (lib "infotab.ss" "setup")
  (define name "Setup PLT")
  (define doc.txt "doc.txt")

  (define compile-omit-files
    (list "setup.ss"))

  (define mzscheme-launcher-libraries (list "setup.ss"))
  (define mzscheme-launcher-names (list "Setup PLT"))

  ;(define mred-launcher-libraries (list "setup.ss"))
  ;(define mred-launcher-names (list "GSetup PLT"))
  )
