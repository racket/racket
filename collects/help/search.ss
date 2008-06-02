#lang scheme/base

(require setup/dirs net/sendurl)
(provide perform-search)

(define search-page "search/index.html")

;; Almost nothing to do here -- the real work is done in the browser,
;; using javascript.
(define (perform-search str)
  (let* ([path (build-path (find-user-doc-dir) search-page)]
         [path (if (file-exists? path)
                 path (build-path (find-doc-dir) search-page))])
    (send-url/file path #:query (format "q=~a" str))))
