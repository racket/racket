(module inf mzscheme
  (provide interface-version timeout start)
  
  (define interface-version 'v1)
  
  (define timeout +inf.0)
  
  ; start : request -> response
  (define (start initial-request) 
    `(html (head (title "A Test Page"))
           (body ([bgcolor "white"])
                 (p "This is a simple module servlet.")))))
