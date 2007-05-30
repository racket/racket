(module suspended-module mzscheme
  (provide interface-version timeout start)
  (require (lib "servlet.ss" "web-server"))
  
  (define interface-version 'v1)
  
  (define timeout (* 7 24 60 60))
  
  ; : request -> response
  (define (start initial-request)
    (let ([name (extract-binding/single
                 'name
                 (request-bindings
                  (send/suspend (let ([question "What is your name?"])
                                  (lambda (k-url)
                                    `(html (head (title ,question))
                                           (body (form ([action ,k-url] [method "post"])
                                                       ,question
                                                       (input ([type "text"] [name "order"]))))))))))])
      `(html (head (title "Hi " ,name "!"))
             (body (p "Hello, " ,name "!  Don't you feel special now?"))))))