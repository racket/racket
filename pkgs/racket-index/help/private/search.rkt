#lang racket/base

(require net/sendurl
         net/uri-codec
         net/url
         racket/string
         setup/dirs
         racket/contract
         racket/list
         scribble/xref
         setup/language-family)

(provide
 (contract-out
  [perform-search
   (->* (any/c)
        (any/c #:language-family (or/c string? #f))
        void?)]
  [send-main-page
   (->* ()
        (#:sub path-string?
         #:fragment (or/c string? #f)
         #:query (or/c string? #f)
         #:notify (-> (or/c string? path?) any)
         #:query-table (hash/c symbol? string? #:immutable #t))
        void?)]
  [send-language-family-page
   (->* ((or/c #f string?))
        ()
        void?)]))

(define search-dir "search/")

;; Almost nothing to do here -- the real work is done in the browser,
;; using javascript.

(define (send-main-page #:sub [sub "index.html"]
                        #:fragment [fragment #f] #:query [query #f]
                        #:notify [notify void]
                        #:query-table [query-table (hash)])
  (define query-table-list-of-pairs
    (for/list ([k (in-list (sort (hash-keys query-table) symbol<?))])
      (cons k (hash-ref query-table k))))
  (define open-url (get-doc-open-url))
  (cond
   [open-url
    (define u (string->url open-url))
    (define dest-url
      (combine-url/relative u
                            (string-join (for/list ([s (explode-path sub)])
                                           (if (path? s) (path-element->string s) (format "~a" s)))
                                         "/")))
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
                                        null)
                                   query-table-list-of-pairs)])))]
    [else
     (define doc-dirs (get-doc-search-dirs))
     (define path (or (and (absolute-path? sub)
                           sub)
                      (for/or ([dir (in-list doc-dirs)])
                        (define path (build-path dir sub))
                        (and (file-exists? path)
                             path))
                      ;; Doesn't exist, but notify and then fall back below:
                      (build-path (find-doc-dir) sub)))
     (notify path)
     (define parsed-query-table
       (if (null? query-table-list-of-pairs)
           #f
           (substring (url->string (url #f #f #f #f #f (list) query-table-list-of-pairs #f))
                      1)))
     (define combined-base-query
       (cond
         [(and query parsed-query-table)
          (string-append query "&" parsed-query-table)]
         [else
          (or query parsed-query-table)]))
     (define combined-query
       (cond
         [(and (not (equal? sub "index.html"))
               (pair? doc-dirs)
               (file-exists? (build-path (car doc-dirs) "index.html")))
          ;; the entry point may or may not try to set `PLT_Root` itself
          (define root (format "PLT_Root=~a" (uri-encode (path->string (path->directory-path (car doc-dirs))))))
          (if combined-base-query
              (string-append combined-base-query "&" root)
              root)]
         [else combined-base-query]))
     (cond
       [(or (file-exists? path) (path? sub))
        (send-url/file path #:fragment fragment #:query combined-query)]
       [else
        (define (part pfx x)
          (if x (string-append pfx x) ""))
        (send-url
         (string-append "https://docs.racket-lang.org/" sub
                        (part "#" fragment)
                        (part "?" combined-query)))])]))

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

(define (perform-search str [context #f]
                        #:language-family [language-family #f])
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
                    query)]
         [fam (and language-family (get-language-family language-family))]
         [famroot (and fam (hash-ref fam 'famroot #f))])
    (send-main-page #:sub (string-append search-dir page)
                    #:query query
                    #:query-table (if fam
                                      (family-query-table fam language-family)
                                      (hash)))))

(define (send-language-family-page name)
  (define fam (get-language-family name))
  (define start-doc (and fam
                         (or (hash-ref fam 'start-doc #f)
                             (hash-ref fam 'doc #f))))
  (define famroot (and fam (hash-ref fam 'famroot #f)))
  (cond
    [start-doc
     (define xref ((dynamic-require 'setup/xref 'load-collections-xref)))
     (define-values (path anchor)
       (xref-tag->path+anchor xref `(part (,(format "~a" start-doc) "top"))))
     (cond
       [path (send-main-page #:sub path #:query-table (family-query-table fam name))]
       [else (send-main-page)])]
    [famroot
     (send-main-page #:sub (format "~a/index.html" famroot) #:query-table (family-query-table fam name))]
    [else
     (send-main-page)]))

(define (get-language-family name)
  (define fams (get-language-families))
  (for/or ([fam (in-list fams)])
    (and (equal? name (hash-ref fam 'fam #f))
         fam)))

(define (family-query-table fam name)
  (define ht (hash 'fam (hash-ref fam 'fam name)))
  (define famroot (hash-ref fam 'famroot #f))
  (if famroot
      (hash-set ht 'famroot famroot)
      ht))
