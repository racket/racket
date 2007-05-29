(module stuff-url mzscheme
  (require (lib "url.ss" "net")
           "utils.ss"
           "mod-map.ss")
  
  ; XXX url: first try continuation, then turn into hash
  
  ; XXX different ways to hash, different ways to store (maybe cookie?)
    
  (provide stuff-url
           stuffed-url?
           extend-url-query
           unstuff-url)
  
  ; XXX Abstract this
  (require (lib "md5.ss"))
  (define (md5-store str)
    (define hash (md5 (string->bytes/utf-8 str)))
    (with-output-to-file
        (build-path (find-system-path 'home-dir) ".urls" (format "~a" hash))
      (lambda ()
        (write str))
      'replace)
    (bytes->string/utf-8 hash))
  (define (md5-lookup hash)
    (with-input-from-file
        (build-path (find-system-path 'home-dir) ".urls" (format "~a" hash))
      (lambda () (read))))
  
  ;; stuff-url: serial url path -> url
  ;; encode in the url
  (define (stuff-url svl uri pth)
    (define result-uri
      (extend-url-query uri 'c (md5-store (write/string (compress-serial svl)))))
    (when (> (string-length (url->string result-uri))
             1024)
      (error "the url is too big: " (url->string result-uri)))
    result-uri)
  
  (define (stuffed-url? uri)
    (let* ([qry (url-query uri)]
           [l-code (find-binding 'c qry)])
      (and l-code
           #t)))
  
  (define (extend-url-query uri key val)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     #t
     (url-path uri)
     (list* (cons key val)
            (url-query uri))
     (url-fragment uri)))
  
  ;; unstuff-url: url url path -> serial
  ;; decode from the url and reconstruct the serial
  (define (unstuff-url req-url ses-url mod-path)
    (decompress-serial (read/string (md5-lookup (find-binding 'c (url-query req-url)))))))