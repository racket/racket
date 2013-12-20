#lang meta/web

(define-context "stubs/packages")

(provide packages)
(define packages
  @page[#:file "" #:link-title "Packages" #:window-title "{{{TITLE}}}"
        #:extra-headers "{{{HEAD}}}"]{
    @; This is a stub page to get the header for planet
    {{{BODY}}}})

(define readme
  @plain[#:file "README"]{
    This directory contains files that are placed here automatically by
    the web build process.  The HTML file contains holes to be filled in
    for the title, header, and body, and the other files are resources
    that are used by the template.})
