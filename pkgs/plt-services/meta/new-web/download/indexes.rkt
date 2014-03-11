#lang plt-web
(require plt-web/indexes
         (only-in "../www/resources.rkt"))

;; Run this module in a directory that contains "docs" as a snapshot
;; of the "docs" directory on download.racket-lang.org, and it will
;; generate "index.html" files to be uploaded to S3 to provide
;; directory-listing "index.html" files where none are present
;; already.
;;
;; The directory doesn't have to be "docs"; it can be any directory
;; at the root of download.racket-lang.org.
;;
;; After the "index.html" files are generated, you have to upload
;; them to S3 manually.

(define download-site (site "download"
                            #:url "http://download.racket-lang.org/"
                            #:always-abs-url? #t))

(make-indexes download-site
              #:use-dir? (lambda (d)
                           (or (not (path? d))
                               (let-values ([(base name dir) (split-path d)])
                                 ;; Don't go into documentation "HTML" directories:
                                 (not (equal? "html" (path->string name)))))))

