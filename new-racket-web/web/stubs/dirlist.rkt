#lang meta/web

;; This stub is to generate fancy directory listings with the Racket style

(require (prefix-in dnld: (only-in "../download/resources.rkt" the-resources)))

(define-context "stubs/dirlist" #:resources dnld:the-resources)

(define header+footer
  (lazy (regexp-split #rx"{{{BODY}}}"
                      (xml->string @page[#:id 'browse-downloads
                                         #:html-only #t
                                         #:part-of 'download
                                         "{{{BODY}}}"]))))

(define header
  @plain[#:file "header.html" #:newline #f (car  (force header+footer))])
(define footer
  @plain[#:file "footer.html" #:newline #f (cadr (force header+footer))])
