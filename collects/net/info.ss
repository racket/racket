(module info (lib "infotab.ss" "setup")
  (define name "Net")
  (define doc.txt "doc.txt")
  
  (define help-desk-message
    "Mz/Mr: See the \"To load\" section of each collection for the command to load it.")
  (define blurb 
    (list
     "The net collection provides a suite of libraries to handle standard "
     "internet-based protocols."))

  ;; Should be removed when openssl is part of main distribution
  (define compile-omit-files '("ssl-tcp-unit.ss")))
