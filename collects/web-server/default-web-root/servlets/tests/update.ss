(module update mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define timeout 60)
  (define interface-version 'v1)
  (define (start initial-request)
    (send/back
     `(html (head)
            (body
             (h1 "Hey"))))))
