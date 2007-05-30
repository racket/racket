(module module-suspended-init mzscheme
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
             (body (p "Hello, " ,name "!  Don't you feel special now?")))))
  
  (send/suspend
   (lambda (k-url)
     `(html (head (title "Module Init"))
            (body (form ([action ,k-url] [method "post"])
                        (p "Maybe calling send/suspend during the module initialization is not a good idea.")
                        (p "This call to send/suspend fails in the development environment since the parameter is #f")
                        (p "It fails in the server because the instance id is not yet installed into the table.")))))))