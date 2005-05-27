
(module dns-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:dns^)
  
  (define-signature net:dns^
    (dns-get-address
     dns-get-name
     dns-get-mail-exchanger
     dns-find-nameserver)))

