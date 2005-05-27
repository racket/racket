(module suspended-module mzscheme
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
             (body (p "Hello, " ,name "!  Don't you feel special now?"))))))