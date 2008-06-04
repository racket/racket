#lang scheme/base

(require setup/dirs net/sendurl net/uri-codec)
(provide perform-search send-main-page)

(define search-page "search/index.html")

;; Almost nothing to do here -- the real work is done in the browser,
;; using javascript.

(define (send-main-page #:sub [sub "index.html"]
                        #:fragment [fragment #f] #:query [query #f])
  (let* ([path (build-path (find-user-doc-dir) sub)]
         [path (if (file-exists? path) path (build-path (find-doc-dir) sub))])
    (send-url/file path #:fragment fragment #:query query)))

(define (perform-search str)
  (send-main-page #:sub search-page #:query (format "q=~a" (uri-encode str))))
