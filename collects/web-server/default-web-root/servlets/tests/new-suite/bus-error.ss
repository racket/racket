(module bus-error mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 120)
  
  (define (end request)
    `(html (body "End")))
  
  (define (start request)
    (send/suspend/callback
     `(html (body (p (a ([href ,start]) "Forward"))
                  (p (a ([href ,end]) "End")))))))
