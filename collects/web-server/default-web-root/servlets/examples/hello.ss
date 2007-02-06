(require (lib "unit.ss")
         (lib "servlet-sig.ss" "web-server"))

(unit (import servlet^)
      (export)
  
  (define the-text "Hello, Web!")
  
  `(html (head (title ,the-text))
         (body ([bgcolor "white"])
               (p ,the-text))))