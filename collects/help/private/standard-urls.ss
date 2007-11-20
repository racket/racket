(module standard-urls mzscheme
  (require (lib "uri-codec.ss" "net")
           (lib "dirs.ss" "setup")
           (lib "contract.ss")
           (lib "config.ss" "planet")
           (lib "help-desk-urls.ss" "help")
           "../servlets/private/util.ss"
           "internal-hp.ss"
           "get-help-url.ss")
  
  (provide home-page-url host+dirs)
  
  (define (search-type? x)
    (member x '("keyword" "keyword-index" "keyword-index-text")))
  
  (define (search-how? x)
    (member x '("exact-match" "containing-match" "regexp-match")))
  
  (define (base-docs-url)
    (if (repos-or-nightly-build?)
      "http://pre.plt-scheme.org/docs"
      (string-append "http://download.plt-scheme.org/doc/" (version))))

  (define (make-docs-plt-url manual-name)
    (format "~a/bundles/~a-doc.plt" (base-docs-url) manual-name))

  (define (make-docs-html-url manual-name)
    (format "~a/html/~a/index.htm" (base-docs-url) manual-name))

  (define (prefix-with-server suffix)
    (format "http://~a:~a~a" internal-host (internal-port) suffix))
  
  (define results-url-prefix (format "http://~a:~a/servlets/results.ss?" internal-host (internal-port)))
  (define flush-manuals-path "/servlets/results.ss?flush=yes")
  (define flush-manuals-url (format "http://~a:~a~a" internal-host (internal-port) flush-manuals-path))
  
  
  (define relative-results-url-prefix "/servlets/results.ss?")

  (define home-page-url (format "http://~a:~a/servlets/home.ss" internal-host (internal-port)))
  
  (define (make-missing-manual-url coll name link)
    (format "http://~a:~a/servlets/missing-manual.ss?manual=~a&name=~a&link=~a"
            internal-host
            (internal-port)
            coll
            (uri-encode name)
            (uri-encode link)))
  
  (define (make-relative-results-url search-string search-type match-type lucky? manuals doc.txt? lang-name)
    (string-append
     relative-results-url-prefix
     (make-results-url-args search-string search-type match-type lucky? manuals doc.txt? lang-name)))

  (define (make-results-url search-string search-type match-type lucky? manuals doc.txt? lang-name)
    (string-append
     results-url-prefix
     (make-results-url-args search-string search-type match-type lucky? manuals doc.txt? lang-name)))
  
  (define (make-results-url-args search-string search-type match-type lucky? manuals doc.txt? language-name)
    (let ([start
           (format
            (string-append "search-string=~a&"
                           "search-type=~a&"
                           "match-type=~a&"
                           "lucky=~a&"
                           "manuals=~a&"
                           "doctxt=~a")
            (uri-encode search-string)
            search-type
            match-type
            (if lucky? "true" "false")
            (uri-encode (format "~s" (map path->bytes manuals)))
            (if doc.txt? "true" "false"))])
      (if language-name
          (string-append start (format "&langname=~a" (uri-encode language-name)))
          start)))
  
  ; sym, string assoc list
  (define hd-locations
    `((hd-tour ,(format "~a/index.html" (get-help-url (build-path (find-doc-dir) "tour"))))
      (release-notes ,url-helpdesk-release-notes)
      (plt-license ,url-helpdesk-license)
      (front-page ,url-helpdesk-home)))
  
  (define hd-location-syms (map car hd-locations))

  (define (get-hd-location sym)
    ; the assq is guarded by the contract
    (cadr (assq sym hd-locations)))
  
  ; host+dirs : (list (cons host-string dir-path))
  ;  association between internal (in normal Helpdesk also virtual) 
  ;  hosts and their corresponding file root.
  (define host+dirs
    (map cons 
         (append collects-hosts  doc-hosts)
         (append collects-dirs   doc-dirs)))
 
  (define (host+file->path host file-path)
    (cond [(assoc host host+dirs)
           => (lambda (internal-host+path)
                (let ([path (cdr internal-host+path)])
                  (build-path path file-path)))]
          [(equal? host "planet")
           (build-path (PLANET-DIR) file-path)]
          [else #f]))

  (provide host+file->path)
  (provide search-type? search-how?)
  (provide/contract 
   (make-relative-results-url (string? 
                               search-type? 
                               search-how?
                               any/c 
                               (listof path?)
                               any/c
                               (or/c false/c string?) . -> . string?))
   (make-results-url (string?
                      search-type? search-how? any/c 
                      (listof path?) 
                      any/c
                      (or/c false/c string?)
                      . -> .
                      string?))
   (flush-manuals-url string?)
   (flush-manuals-path string?)
   (make-missing-manual-url (string? string? string? . -> . string?))
   (get-hd-location ((lambda (sym) (memq sym hd-location-syms))
                     . -> . 
                     string?))
   [prefix-with-server (string? . -> . string?)]
   [make-docs-plt-url (string? . -> . string?)]
   [make-docs-html-url (string? . -> . string?)]))
