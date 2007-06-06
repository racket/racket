(module stuff-url mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "serialize.ss")
           "../private/util.ss"
           "../private/url-param.ss"
           "../private/mod-map.ss")
  
  ; XXX url: first try continuation, then turn into hash
  
  ; XXX different ways to hash, different ways to store (maybe cookie?)
    
  (provide/contract
   [stuff-url (serializable? url? . -> . url?)]
   [stuffed-url? (url? . -> . boolean?)]
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
      (insert-param uri "c" (md5-store (write/string (compress-serial (serialize svl))))))
    (when (> (string-length (url->string result-uri))
             1024)
      (error "the url is too big: " (url->string result-uri)))
    result-uri)
  
  (define (stuffed-url? uri)
    (and (extract-param uri "c")
         #t))
  
  ;; unstuff-url: url -> serial
  ;; decode from the url and reconstruct the serial
  (define (unstuff-url req-url)
    (deserialize (decompress-serial (read/string (md5-lookup (extract-param req-url "c")))))))