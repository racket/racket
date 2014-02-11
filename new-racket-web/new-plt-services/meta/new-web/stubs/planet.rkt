#lang plt-web

(define planet-site (site "stubs/planet"
                          #:url "http://planet.racket-lang.org/"
                          #:always-abs-url? #t))

(provide planet)
(define planet
  @page[#:site planet-site
        #:file "" #:link-title "PLaneT" #:window-title "{{{TITLE}}}"
        #:extra-headers "{{{HEAD}}}"]{
    @; This is a stub page to get the header for planet
    {{{BODY}}}})

(define readme
  @plain[#:site planet-site #:file "README"]{
    This directory contains files that are placed here automatically by
    the web build process.  The HTML file contains holes to be filled in
    for the title, header, and body, and the other files are resources
    that are used by the template.})
