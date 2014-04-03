#lang plt-web

;; This stub is to generate fancy directory listings with the Racket style

(require (only-in "../download/resources.rkt" download-site))

(define dirlist-site (site "stubs/dirlist" 
                           #:url "http://download.racket-lang.org/"
                           #:always-abs-url? #t
                           #:share-from download-site))

(define header+footer
  (lazy (regexp-split #rx"{{{BODY}}}"
                      (xml->string @page[#:site dirlist-site
                                         #:id 'browse-downloads
                                         #:html-only? #t
                                         #:part-of 'download
                                         "{{{BODY}}}"]))))

(define header
  @plain[#:site dirlist-site
         #:file "header.html" #:newline #f (car  (force header+footer))])
(define footer
  @plain[#:site dirlist-site
         #:file "footer.html" #:newline #f (cadr (force header+footer))])
