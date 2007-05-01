(module instance-expiration mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define timeout 5)
  (define interface-version 'v2)
  
  (define (instance-expiration-handler expired-request)
    ; Not allowed to call any (lib "servlet.ss" "web-server") methods
    ; (I can't enforce this, however, so if you accidentally do weird things will happen.)
    `(html (head (title "You win.")) (body "You win.")))
  
  (define (start initial-request)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html
         (head (title "Instance expiration demo"))
         (body (p (a ([href ,(embed/url 
                              (lambda (request)
                                `(html (head (title "Instance expiration demo"))
                                       (body (p "Reload in a few minutes.")
                                             (p "(or change the instance id to something made up.")))))])
                     "Click this link."))))))))
