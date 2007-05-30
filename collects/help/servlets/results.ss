#|

Since the web server is set up to have a separate namespace for each
servlet, this servlet must be able to both use and flush the documentation
index cache. Flushing the cache elsewhere will not dump it, since the cache
is stored in a module top-level and that's namespace-specific.

|#

(module results mzscheme
  (require (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "servlet.ss" "web-server")
           (lib "uri-codec.ss" "net")
	   (lib "dirs.ss" "setup")
           "../private/internal-hp.ss"
           "../private/path.ss"
           "../private/docpos.ss"
           "../private/search.ss"
           "../private/manuals.ss"
           "../private/get-help-url.ss"
           (lib "string-constant.ss" "string-constants")
           "private/util.ss"
           "private/search-util.ss"
           "private/headelts.ss")
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       (let ()
         ;; doc subcollection name -> boolean
         (define (search-type->search-level st)
           (let loop ([n 0] [lst (map car search-types)])
             (when (null? lst) (raise 'bad-search-type))
             (if (string=? (car lst) st) n (loop (add1 n) (cdr lst)))))
         
         (define search-responses #f)
         
         ;; from what I can tell, this variable doesn't work as intended.
         ;; I've left it in for now, but this whole file needs to be rewritten.
         ;; -robby
         (define current-kind #f)
         
         (define last-header #f)
         
         (define max-reached #f)
         (define (build-maxxed-out k)
           (lambda ()
             (unless max-reached
               (set! max-reached #t)
               (set! search-responses
                     (cons `(b ,(with-color
                                 "red"
                                 (string-constant
                                  plt:hd:search-stopped-too-many-matches)))
                           search-responses)))
             (k #f)))
         
         (define (add-header s key)
           (unless max-reached
             (set! last-header s)
             (set! search-responses
                   (list* `(b ([style "font-family:Verdana,Helvetica,sans-serif"])
                              ,s)
                          `(br)
                          search-responses))))
         
         (define (set-current-kind! s key)
           (set! current-kind (cadr (assoc s kind-types))))
         
         (define exp-web-root
           (explode-path (normalize-path (find-collects-dir))))
         (define web-root-len (length exp-web-root))
         
         (define (keyword-string? ekey)
           (and (string? ekey)
                (not (string=? ekey ""))))
         
         (define (pretty-label label ekey)
           (if (keyword-string? ekey)
               `(font ([face "monospace"])
                      ;; boldface keyword occurrences
                      ,@(let ([mpos (regexp-match-positions (non-regexp ekey) label)])
                          (if mpos
                              (let* ([item (car mpos)]
                                     [start (car item)]
                                     [stop (cdr item)])
                                (list (substring label 0 start)
                                      `(b ,(substring label start stop))
                                      (substring label stop (string-length label))))
                              (list label))))
               label))
         
         (define (maybe-extract-coll s)
           (let ([len (string-length s)])
             (if (and (> len 17)
                      (string=? (substring s 0 4) "the ")
                      (string=? (substring s (- len 11) len) " collection"))
                 (substring s 4 (- len 11))
                 s)))
         
         (define no-anchor-format
           (string-append "/servlets/doc-anchor.ss?"
                          "file=~a&"
                          "caption=~a&"
                          "name=~a"))
         
         (define with-anchor-format
           (string-append no-anchor-format "&offset=~a#temp"))
         
         (define (make-caption coll)
           (format "Documentation for the ~a collection" coll))
         
         (define (make-search-link href label src ekey)
           `(table ([cellspacing "0"] [cellpadding "0"])
                   (tr (td (div ([align "left-outdent"])
                                (a ([href ,href]) ,(pretty-label label ekey))
                                " in \"" ,src "\"")))))
         
         ;; doc-txt? : string -> boolean
         (define (doc-txt? str) (regexp-match "doc\\.txt$" str))
         
         (define (make-html-href page-label path)
           (let ([anchored-path (make-anchored-path page-label path)])
             (cond [(servlet-path? path) anchored-path]
                   [(doc-txt? (path->string path)) ; collection doc.txt
                    (let ([maybe-coll (maybe-extract-coll last-header)])
                      (format no-anchor-format
                              (uri-encode anchored-path)
                              (uri-encode (make-caption maybe-coll))
                              maybe-coll))]
                   [else ; manual, so have absolute path
                    (get-help-url path page-label)])))
         
         ;; make-anchored-path : string path -> string
         ;; page-label is #f or a bytes that labels an HTML anchor
         ;; path is either an absolute pathname (possibly not normalized) 
         ;; in the format of the native OS, or, in the case of Help Desk 
         ;; servlets, a forward-slashified path beginning with "/servlets/"
         (define (make-anchored-path page-label path)
           (let ([normal-path
                  (if (servlet-path? path)
                      path
                      (normalize-path path))])
             (if (and page-label
                      (string? page-label)
                      (not (or (string=? page-label "NO TAG")
                               (regexp-match "\\?|&" page-label))))
                 (string-append (path->string normal-path) "#" page-label)
                 (path->string normal-path))))
         
         ; path is absolute pathname
         (define (make-text-href page-label path)
           (let* ([maybe-coll (maybe-extract-coll last-header)]
                  [hex-path (uri-encode (path->string (normalize-path path)))]
                  [hex-caption (if (eq? maybe-coll last-header)
                                   hex-path
                                   (uri-encode (make-caption maybe-coll)))]
                  [offset (or (and (number? page-label) page-label)
                              0)])
             (format with-anchor-format
                     hex-path hex-caption (uri-encode maybe-coll) offset)))
         
         (define (html-entry? path)
           (and (not (suffixed? path #"doc.txt"))
                (or (eq? current-kind 'html) (suffixed? path #".html"))))
         
         (define (suffixed? path suffix)
           (let* ([path-bytes (path->bytes path)]
                  [path-len (bytes-length path-bytes)]
                  [suffix-len (bytes-length suffix)])
             (and (path-len . >= . suffix-len)
                  (bytes=? (subbytes path-bytes (- path-len suffix-len) path-len)
                           suffix))))
         
         (define (goto-lucky-entry ekey label src path page-label key)
           (let ([href (if (html-entry? path)
                           (make-html-href page-label path)
                           (make-text-href page-label path))])
             (send/finish (redirect-to href))))
         
         (define (add-entry ekey label src path page-label key)
           (let* ([entry
                   (if (html-entry? path)
                       (make-search-link (make-html-href page-label path)
                                         label src ekey)
                       (make-search-link (make-text-href page-label path)
                                         label src ekey))])
             (set! search-responses (cons entry search-responses))))
         
         (define (make-results-page search-string lang-name items regexp? exact?)
           (let-values ([(string-finds finds)
                         (build-string-finds/finds search-string regexp? exact?)])
             `(html
               (head ,hd-css ,@hd-links (title "PLT Help Desk search results"))
               (body
                (h1 "Search Results")
                (h2
                 ,@(if lang-name
                       (list "Language: " (with-color "firebrick" lang-name) '(br))
                       '())
                 ,@(let ([single-key
                          (lambda (sf)
                            (with-color "firebrick" (format " \"~a\"" sf)))])
                     (cond [(null? string-finds) '()]
                           [(null? (cdr string-finds))
                            (list "Key: " (single-key (car string-finds)))]
                           [else
                            (cons "Keys: " (map single-key string-finds))])))
                (br)
                ,@items))))
         
         (define (search-results lucky? search-string search-type match-type
                                 manuals doc-txt? lang-name)
           (set! search-responses '())
           (set! max-reached #f)
           (let* ([search-level (search-type->search-level search-type)]
                  [regexp? (string=? match-type "regexp-match")]
                  [exact-match? (string=? match-type "exact-match")]
                  [key (gensym)]
                  [result (let/ec k
                            (do-search search-string
                                       search-level
                                       regexp?
                                       exact-match?
                                       manuals
                                       doc-txt?
                                       key
                                       (build-maxxed-out k)
                                       add-header
                                       set-current-kind!
                                       (if lucky? goto-lucky-entry add-entry)))]
                  [html (make-results-page
                         search-string
                         lang-name
                         (if (string? result) ; error message
                             `((h2 ([style "color:red"]) ,result))
                             (reverse search-responses))
                         regexp?
                         exact-match?)])
             html))
         
         (define empty-search-page
           `(html (head (title "Empty search string in PLT Help Desk"))
                  (body (h2 "Empty search string"))))
         
         (define (lucky-search? bindings)
           (with-handlers ([exn:fail? (lambda _ #f)])
             (let ([result (extract-binding/single 'lucky bindings)])
               (not (string=? result "false")))))
         
         (define (maybe-update-box b s)
           (unless (string=? s "") (set-box! b s)))
         
         (define (convert-manuals manuals)
           (if manuals
               (let ([parsed (read-from-string manuals)])
                 (if (and (list? parsed) (andmap bytes? parsed))
                     (map bytes->path parsed)
                     (map car (find-doc-names))))
               (map car (find-doc-names))))
         
         (let* ([bindings (request-bindings initial-request)]
                [maybe-get (lambda (sym)
                             (with-handlers ([exn:fail?
                                              (lambda (_) #f)])
                               (extract-binding/single sym bindings)))]
                [flush (maybe-get 'flush)])
           (cond
             [flush
              (doc-collections-changed)
              `(html (head (title "Flush"))
                     (body (h2 "Flushed documentation cache")))]
             [else
              (let ([search-string (maybe-get 'search-string)]
                    [search-type (maybe-get 'search-type)]
                    [match-type (maybe-get 'match-type)]
                    [manuals (maybe-get 'manuals)]
                    [doc.txt (maybe-get 'doctxt)]
                    [lang-name (maybe-get 'langname)])
                (if (or (not search-string) (= (string-length search-string) 0))
                    empty-search-page
                    (search-results (lucky-search? bindings)
                                    search-string
                                    (or search-type "keyword-index")
                                    (or match-type "containing-match")
                                    (convert-manuals manuals)
                                    (cond [(not doc.txt) #t]
                                          [(equal? doc.txt "false") #f]
                                          [else #t])
                                    lang-name)))])))))))
