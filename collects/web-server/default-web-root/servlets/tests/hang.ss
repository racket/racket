(module hang mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  
  (define (start initial-request)
    (send/suspend
     (lambda (k-url)
       `(html (a ([href ,k-url]) "Next"))))
    (send/suspend
     (lambda (k-url)
       `(html (a ([href ,k-url]) "Error"))))
    (/ 1 0)))
