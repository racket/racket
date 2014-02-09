#lang plt-web

(require (only-in "../www/resources.rkt" www-site))

(define wiki-site (site "stubs/wiki" #:resources (site-resources www-site)))

(define template
  (page #:site wiki-site
        #:title "{{{TITLE}}}"
        #:extra-headers "{{{HEADERS}}}"
        #:extra-body-attrs '(|{{{ATTRS}}}|: #t)
        "{{{BODY}}}"))
