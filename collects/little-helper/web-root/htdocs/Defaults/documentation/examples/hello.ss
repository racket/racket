(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(unit/sig () (import servlet^)
  
  (define the-text "Hello, Web!")
  
  `(html (head (title ,the-text))
         (body ([bgcolor "white"])
               (p ,the-text))))