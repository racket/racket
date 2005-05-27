(module b-module mzscheme
  (provide interface-version timeout start)
  (require (lib "servlet-sig.ss" "web-server"))
  
  (define interface-version 'v1)
  
  (define timeout +inf.0)
  
  ; start : request -> response
  (define (start initial-request)
    `(html (head (title "A Test Page"))
           (body ([bgcolor "white"])
                 (p ,(format "Here are the initial bindings: ~v"
                             (request-bindings initial-request)))))))