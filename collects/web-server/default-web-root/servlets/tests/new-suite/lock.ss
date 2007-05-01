(module lock mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v2-transitional)
  (define timeout 60)
  (define (instance-expiration-handler _)
    `(html (body "Error")))
  (define (start _)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (body 
               (p (a ([href ,(embed/url
                              (lambda _
                                (sleep 5)
                                (second "Slow")))])
                     "Slow"))
               (p (a ([href ,(embed/url
                              (lambda _
                                (second "Fast")))])
                     "Fast")))))))
  (define (second label)
    `(html (body ,label
                 ,(number->string (current-seconds))))))
