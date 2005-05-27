;; copyright by Paul Graunke June 2000 AD

(module sgml-reader-sig mzscheme
  (require (lib "unitsig.ss"))

  (define-signature sgml-reader^ (read-comments trim-whitespace gen-may-contain gen-read-sgml))
  
  (provide sgml-reader^))
