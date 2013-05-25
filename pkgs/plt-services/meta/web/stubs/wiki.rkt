#lang meta/web

(require (prefix-in www: (only-in "../www/resources.rkt" the-resources)))

(define-context "stubs/wiki" #:resources www:the-resources)

(define template
  (page #:title "{{{TITLE}}}"
        #:extra-headers "{{{HEADERS}}}"
        #:extra-body-attrs '(|{{{ATTRS}}}|: #t)
        "{{{BODY}}}"))
