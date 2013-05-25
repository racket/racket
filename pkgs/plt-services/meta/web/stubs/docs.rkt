#lang meta/web

(require (prefix-in www: (only-in "../www/resources.rkt" the-resources)))

(define-context "stubs/docs" #:resources www:the-resources)

(provide documentation)
(define documentation
  ;; This is a stub page to get the header for tweaked doc pages
  (page #:file "" #:link-title "Documentation" #:window-title "{{{TITLE}}}"
        "\n{{{BODY}}}\n"))
