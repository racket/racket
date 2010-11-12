#lang meta/web

(require (prefix-in www: (only-in "../www/resources.rkt" the-resources)))

(define-context "stubs/wiki" #:resources www:the-resources)

(define header+footer
  (delay (regexp-split #rx"{{{BODY}}}"
                       (xml->string @page[#:id 'browse-downloads
                                          #:html-only #t
                                          #:part-of 'download
                                          "{{{BODY}}}"]))))

(define template
  @page[#:title "{{{TITLE}}}" "{{{BODY}}}"])
