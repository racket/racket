(module hello mzscheme
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (define the-text "Hello, Web!")
    
    `(html (head (title ,the-text))
           (body ([bgcolor "white"])
                 (p ,the-text)))))