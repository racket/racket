(module bad-xexpr mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  (define (start initial-request)
    (send/back
     `(html (a ([href url])
               "Title")))))
