(module pr5490 mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 120)
  (define (start ireq)
    (send/finish '(("paul") "..."))))
