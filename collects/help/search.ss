#lang scheme/base

(require setup/dirs net/sendurl net/uri-codec)
(provide perform-search send-main-page)

(define search-dir "search/")

;; Almost nothing to do here -- the real work is done in the browser,
;; using javascript.

(define (send-main-page #:sub [sub "index.html"]
                        #:fragment [fragment #f] #:query [query #f])
  (let* ([path (build-path (find-user-doc-dir) sub)]
         [path (if (file-exists? path) path (build-path (find-doc-dir) sub))])
    (send-url/file path #:fragment fragment #:query query)))

(define (perform-search str [context #f])
  (let* ([page (if context "search-context.htm" "index.html")]
         [query (format "q=~a" (uri-encode str))]
         [query (if context
                  (format "~a&hq=~a" query (uri-encode context))
                  query)])
    (send-main-page #:sub (string-append search-dir page) #:query query)))
