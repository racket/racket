#lang setup/infotab
(require string-constants)

(define name "PLAI")
(define blurb '("Language levels for the Programming Languages: Application and Interpretation textbook"))
(define homepage "http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/")
(define primary-file "main.ss")
  
(define scribblings '(("scribblings/plai.scrbl" () (language -11))))

(define textbook-pls
  (list (list '("plai-small.gif" "plai")
              "Programming Languages: Application and Interpretation"
              (string-constant teaching-languages)
              "Programming Languages: Application and Interpretation")))

(define tools (list "plai-tool.ss"))
(define tool-icons (list "plai-small.gif"))
(define tool-names 
  (list "Programming Languages: Application and Interpretation"))
(define tool-urls 
  (list "http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/"))
