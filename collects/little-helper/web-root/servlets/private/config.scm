(module config mzscheme
  (provide base-url
           external-style-sheet
           port)
  
  (define port 8014)
  (define base-url (format "http://localhost:~a" port))
  (define external-style-sheet "scribble.css"))
