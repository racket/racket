
(module info (lib "infotab.ss" "setup")
  (define doc.txt "doc.txt")
  (define name "mzc")

  (define mzscheme-launcher-libraries (list "start.ss"))
  (define mzscheme-launcher-names (list "mzc"))
  (define mred-launcher-libraries (list "start.ss"))
  (define mred-launcher-names (list "gmzc"))

  (define compile-omit-files
    '("mrspidey.ss" "mrspideyf.ss" "mrspideyi.ss" "embedr.ss")))
