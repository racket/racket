#lang scheme
(require net/url
         scheme/serialize
         web-server/private/md5-store
         web-server/private/gzip
         "../private/util.ss"
         "../private/url-param.ss"
         "../private/mod-map.ss")

(provide/contract
 [max-url-length (parameter/c number?)]
 [url-too-big? (url? . -> . boolean?)]
 [stuff-url (serializable? url? . -> . url?)]
 [stuffed-url? (url? . -> . boolean?)]
 [unstuff-url (url? . -> . serializable?)])

; http://www.boutell.com/newfaq/misc/urllength.html
(define max-url-length
  (make-parameter 2048))

(define (url-too-big? uri)
  ((string-length (url->string uri)) . > . (max-url-length)))

;; stuff-url: serial url -> url
;; encode in the url
(require net/base64)
(define (stuff-url c uri)
  (let* ([cb (c->bytes c)]
         [cb-uri (insert-param uri "c" (bytes->string/utf-8 cb))])
    (if (url-too-big? cb-uri)
        (let* ([cc (gzip/bytes cb)]
               [cc-uri (insert-param uri "cc" (bytes->string/utf-8 (base64-encode cc)))])
          (if (url-too-big? cc-uri)
              (let* ([hc (md5-store cc)]
                     [hc-uri (insert-param uri "hc" (bytes->string/utf-8 hc))])
                (if (url-too-big? hc-uri)
                    (error 'stuff-url "Continuation too big: ~a" c)
                    hc-uri))
              cc-uri))
        cb-uri)))

(define (stuffed-url? uri)
  (and (or (extract-param uri "c")
           (extract-param uri "cc")
           (extract-param uri "hc"))
       #t))

(define (c->bytes c)
  (write/bytes (compress-serial (serialize c))))
(define (bytes->c b)
  (deserialize (decompress-serial (read/bytes b))))

;; unstuff-url: url -> serial
;; decode from the url and reconstruct the serial
(define (unstuff-url uri)
  (cond
    [(extract-param uri "c") 
     => (compose bytes->c string->bytes/utf-8)]
    [(extract-param uri "cc")
     => (compose bytes->c gunzip/bytes base64-decode string->bytes/utf-8)]
    [(extract-param uri "hc")
     => (compose bytes->c gunzip/bytes md5-lookup string->bytes/utf-8)]))