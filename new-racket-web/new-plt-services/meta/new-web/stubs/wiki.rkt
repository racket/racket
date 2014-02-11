#lang plt-web

(require (only-in "../www/resources.rkt" www-site))

(define wiki-site (site "stubs/wiki"
                        #:url "http://wiki.racket-lang.org/"
                        #:always-abs-url? #t
                        #:share-from www-site))

(define template
  (page #:site wiki-site
        #:title "{{{TITLE}}}"
        #:extra-headers "{{{HEADERS}}}"
        #:extra-body-attrs '(|{{{ATTRS}}}|: #t)
        "{{{BODY}}}"))
