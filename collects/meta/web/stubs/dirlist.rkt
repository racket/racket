#lang at-exp s-exp "../common.rkt"

;; This stub is to generate fancy directory listings with the Racket style

(require (prefix-in dnld: (only-in "../download/shared.rkt" the-resources)))

(define-context "stubs/dirlist" #:resources dnld:the-resources)

(define header+footer
  (delay (regexp-split #rx"{{{BODY}}}"
                       (xml->string @page[#:id 'browse-downloads
                                          #:html-only #t
                                          "{{{BODY}}}"]))))

(define header @plain[#:file "header.html" (car  (force header+footer))])
(define footer @plain[#:file "footer.html" (cadr (force header+footer))])
