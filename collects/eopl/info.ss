#lang setup/infotab

(require string-constants)

(define tools (list "eopl-tool.ss"))
(define tool-icons (list "eopl-small.gif"))
(define tool-names (list "Essentials of Programming Languages"))
(define tool-urls (list "http://www.cs.indiana.edu/eopl/"))

(define scribblings '(("eopl.scrbl" () (language -12))))

(define textbook-pls
  (list (list '("eopl-small.gif" "eopl")
              "Essentials of Programming Languages"
              (string-constant teaching-languages)
              "Essentials of Programming Languages (2nd ed.)")))
