(module module-suspended-init mzscheme
  (provide interface-version timeout start)
  (require (lib "servlet.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "servlet-sig.ss" "web-server"))
  
  (define interface-version 'v1)
  
  (define timeout (* 7 24 60 60))
  
  ; : request -> response
  (define (start initial-request)
    (let ([name (extract-binding/single
                 'name
                 (request-bindings
                  (send/suspend (let ([question "What is your name?"])
                                  (build-suspender
                                   `(,question)
                                   `(,question (input ([type "text"] [name "name"]))))))))])
      `(html (head (title "Hi " ,name "!"))
             (body (p "Hello, " ,name "!  Don't you feel special now?")))))
  
  (send/suspend
   (build-suspender '("Module Init")
                    '((p "Maybe calling send/suspend during the module initialization is not a good idea.")
                      (p "This call to send/suspend fails in the development environment since the parameter is #f")
                      (p "It fails in the server because the instance id is not yet installed into the table.")))))