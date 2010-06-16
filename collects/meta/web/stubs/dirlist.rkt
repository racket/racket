#lang at-exp s-exp "../common.rkt"

;; This stub is to generate fancy directory listings with the Racket style

(define-context "stubs/dirlist")

(require racket/port)
(define (xml->string content)
  (regexp-replace* #rx"&nbsp;"
                   (with-output-to-string (lambda () (output-xml content)))
                   "\\&#160;"))

;; (define (racket-navbar)  (xml->string (www:the-resources 'navbar #f)))
;; (define (racket-favicon) (xml->string (www:the-resources 'favicon-headers)))

(define header+footer
  (delay (regexp-split #rx"{{{BODY}}}"
                       (xml->string @page[#:id 'browse-downloads
                                          #:html-only #t
                                          "{{{BODY}}}"]))))

(define header @plain[#:file "header.html" (car  (force header+footer))])
(define footer @plain[#:file "footer.html" (cadr (force header+footer))])
