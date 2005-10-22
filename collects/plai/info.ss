(module info (lib "infotab.ss" "setup")
  (require (lib "string-constant.ss" "string-constants"))
  
  (define name "PLAI")
  (define doc.txt "doc.txt")
  (define tools (list "plai-tool.ss"))
  (define tool-names (list "Programming Languages: Application and Interpretation"))
  (define tool-urls (list "http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/"))
  
  (define textbook-pls
    (list (list '("plai-icon.png" "plai")
                "Programming Languages: Application and Interpretation"
                (string-constant teaching-languages)
                "Programming Languages: Application and Interpretation"
                "PLAI - Beginning Student"))))
