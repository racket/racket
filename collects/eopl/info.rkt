#lang setup/infotab

(require string-constants)

(define tools (list "eopl-tool.ss"))
(define tool-icons (list "eopl-small.png"))
(define tool-names (list "Essentials of Programming Languages"))
(define tool-urls (list "http://www.eopl3.com/"))

(define scribblings '(("eopl.scrbl" () (language -12))))

(define textbook-pls
  (list (list '("eopl-small.png" "eopl")
              "Essentials of Programming Languages"
              (string-constant teaching-languages)
              "Essentials of Programming Languages (3rd ed.)")))
