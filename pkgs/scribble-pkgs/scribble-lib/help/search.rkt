#lang racket/base

(require setup/dirs
         net/sendurl
         net/uri-codec
         net/url
         racket/string)
(provide perform-search send-main-page)

(define search-dir "search/")

;; Almost nothing to do here -- the real work is done in the browser,
;; using javascript.

(define (send-main-page #:sub [sub "index.html"]
                        #:fragment [fragment #f] #:query [query #f]
                        #:notify [notify void])
  (define open-url (get-doc-open-url))
  (cond
   [open-url
    (define dest-url (let ([u (string->url open-url)])
                       (combine-url/relative
                        u
                        (string-join
                         (for/list ([s (explode-path sub)])
                           (if (path? s)
                               (path-element->string s)
                               (format "~a" s)))
                         "/"))))
    (notify (url->string dest-url))
    (send-url (url->string
               (struct-copy url dest-url
                            [fragment (or fragment
                                          (url-fragment dest-url))]
                            [query (append
                                    (url-query dest-url)
                                    (if query
                                        (url-query
                                         (string->url
                                          (format "q?~a" query)))
                                        null))])))]
   [else
    (let* ([path (build-path (find-user-doc-dir) sub)]
           [path (if (file-exists? path) path (build-path (find-doc-dir) sub))])
      (notify path)
      (send-url/file path #:fragment fragment #:query query))]))

;; This is an example of changing this code to use the online manuals.
;; Normally, it's better to set `doc-open-url` in "etc/config.rktd",
;; but as a last resort, you can change `send-main-page` to compute a URL.
;; This may be useful in cases like schools that use systems that have problems
;; running a browser on local files (like NEU).  If you use this, then
;; it is a good idea to put the documentation tree somewhere local, to
;; have better interaction times instead of using the PLT server.
;; (define (send-main-page #:sub [sub "index.html"]
;;                         #:fragment [fragment #f] #:query [query #f]
;;                         #:notify [notify void])
;;   (define (part pfx x) (if x (string-append pfx x) ""))
;;   (send-url (string-append
;;              "http://download.racket-lang.org/docs/" (version) "/html/"
;;              sub (part "#" fragment) (part "?" query))))

(define (perform-search str [context #f])
  ;; `context' can be a pre-filter query string to use for a context,
  ;; optionally a list of one and a label to display for that context.
  ;; In any case, when a context is specified, the search actually
  ;; goes through the search-context.html page which tranpolines to
  ;; the main search page after setting the cookies (so when the
  ;; search page is refreshed it won't reset the context).
  (let* ([label   (and (list? context) (= 2 (length context)) (cadr context))]
         [context (if (pair? context) (car context) context)]
         [page    (if context "search-context.html" "index.html")]
         [query   (format "q=~a" (uri-encode str))]
         [query   (if context
                    (format "~a&hq=~a~a"
                            query (uri-encode context)
                            (if label
                              (format "&label=~a" (uri-encode label))
                              ""))
                    query)])
    (send-main-page #:sub (string-append search-dir page) #:query query)))
