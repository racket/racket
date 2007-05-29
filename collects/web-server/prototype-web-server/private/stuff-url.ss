(module stuff-url mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "serialize.ss")
           "utils.ss"
           "mod-map.ss")
  
  ; XXX url: first try continuation, then turn into hash
  
  ; XXX different ways to hash, different ways to store (maybe cookie?)
    
  (provide/contract
   [stuff-url (serializable? url? . -> . url?)]
   [stuffed-url? (url? . -> . boolean?)]
   [extend-url-query (url? symbol? string? . -> . url?)]
   [unstuff-url (url? . -> . serializable?)])
  
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
  
  ;; stuff-url: serial url -> url
  ;; encode in the url
  (define (stuff-url svl uri)
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
  
  ;; unstuff-url: url -> serial
  ;; decode from the url and reconstruct the serial
  (define (unstuff-url req-url)
    (decompress-serial (read/string (md5-lookup (find-binding 'c (url-query req-url)))))))