;; info.ss for srpersist collection

;; no .zo compilation necessary, since all the
;; real code is in C++

(module info (lib "infotab.ss" "setup")
  (define name "SrPersist")
  (define doc.txt "doc.txt")
  (define help-desk-message
     "Mz/Mr: (require (lib \"srpersist.ss\" \"srpersist\"))")
  (define compile-omit-files
    '("info.ss"
      "srpersist.ss"))
  (define blurb
    (list
      "SrPersist is an extension for using ODBC databases from Scheme.")))
