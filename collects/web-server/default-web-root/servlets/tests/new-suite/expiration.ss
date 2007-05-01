(module expiration mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define timeout (* 60 3))
  (define interface-version 'v1)
  
  (define (start initial-request)
    (parameterize ([current-servlet-continuation-expiration-handler
                    (lambda (request-for-expired)
                      (send/back
                       `(html (body (p "You lose! (Default)")))))])
      (let loop ([request initial-request])
        (send/suspend/dispatch
         (lambda (embed/url)
           `(html
             (head (title "Expiration demo"))
             (body (p "Open each of the links below in a new window. Then click the link in 'Forget' window. Then reload each window.")
                   (p (a ([href ,(embed/url loop)])
                         "Loop"))
                   (p (a ([href ,(embed/url 
                                  loop
                                  (lambda (request-for-expired)
                                    (send/back
                                     `(html (head (title "Expiration demo"))
                                            (body (p "You win! (Special)"))))))])
                         "Loop w/ Expiration"))
                   (p (a ([href ,(embed/url
                                  (lambda (request)
                                    (loop 
                                     (send/forward
                                      (lambda (k-url)
                                        `(html (head (title "Expiration demo"))
                                               (body (p (a ([href ,k-url]) "Forget the past.")))))))))])
                         "Prepare to forget the past."))))))))))
