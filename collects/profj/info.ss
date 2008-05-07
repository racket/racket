#lang setup/infotab

(require string-constants)
(define name "ProfessorJ")
(define tools (list (list "tool.ss") #;(list "test-tool.ss")))
(define tool-names '("ProfessorJ" #;"ProfessorJ Testing"))
(define install-collection "installer.ss")
(define pre-install-collection "pre-installer.ss")
(define textbook-pls
  (list (list '("htdch-icon.png" "profj")
              "How to Design Classes"
              (string-constant experimental-languages)
              "ProfessorJ"
              "Beginner")))
(define scribblings '(("scribblings/htdc.scrbl" (multi-page) (language -10.5))))
