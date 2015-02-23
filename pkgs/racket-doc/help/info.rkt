#lang info

(define post-install-collection "installer.rkt")
(define raco-commands '(("docs" help/help "search and view documentation" 110)))

(define scribblings '(("help.scrbl")))

(define copy-man-pages '("plt-help.1"))

(define test-responsibles '((all robby)))
