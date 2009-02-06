#lang scheme
(require net/url
         scheme/serialize
         web-server/private/md5-store
         web-server/private/gzip
         web-server/private/servlet
         web-server/http
         "../private/util.ss"
         "../private/url-param.ss"
         "../private/mod-map.ss")

;; NEW
; A stuffer is
;  - in  : any -> bytes
;  - out : bytes -> any
; such that
;  out (in x) = x
;  in (out x) = x
(define-struct stuffer (in out))

(define id-stuffer
  (make-stuffer 
   (lambda (v) v)
   (lambda (v) v)))

(define serialize-stuffer
  (make-stuffer
   (lambda (v) (write/bytes (compress-serial (serialize v))))
   (lambda (v) (deserialize (decompress-serial (read/bytes v))))))

(define gzip-stuffer
  (make-stuffer gzip/bytes gunzip/bytes))

(require net/base64)
(define base64-stuffer
  (make-stuffer base64-encode base64-decode))

(define-struct store (write read))
(define (dir-store home)
  (make-store
   (lambda (key value)
     (with-output-to-file
         (build-path home (format "~a" key))
       (lambda ()
         (write value))
       #:exists 'replace))
   (lambda (key)
     (with-input-from-file
         (build-path home (format "~a" key))
       (lambda () (read))))))

(define (hash-stuffer hash store)
  (make-stuffer
   (lambda (v)
     (define hv (hash v))
     ((store-write store) hv v)
     hv)
   (lambda (hv)
     ((store-read store) hv))))

(require file/md5)
(define (md5-stuffer home)
  (hash-stuffer md5 (dir-store home)))

(define (stuffer-compose g f)
  (make-stuffer
   (lambda (v)
     ((stuffer-in g) ((stuffer-in f) v)))
   (lambda (v)
     ((stuffer-out f) ((stuffer-out g) v)))))

(define (stuffer-sequence f g)
  (stuffer-compose g f))

(define (stuffer-if c f)
  (make-stuffer
   (lambda (v)
     (if (c v)
         (bytes-append #"1" ((stuffer-in f) v))
         (bytes-append #"0" v)))
   (lambda (tv)
     (define tag (subbytes tv 0 1))
     (define v (subbytes tv 1))
     (if (bytes=? tag #"1")
         ((stuffer-out f) v)
         v))))

(define (stuffer-chain . ss)
  (match ss
    [(list)
     id-stuffer]
    [(list-rest f ss)
     (cond
       [(stuffer? f)     
        (stuffer-sequence
         f (apply stuffer-chain ss))]
       [(procedure? f)
        (stuffer-if 
         f (apply stuffer-chain ss))])]))

(define (is-url-too-big? v)
  (define uri
    (request-uri (execution-context-request (current-execution-context))))
  (url-too-big?
   (do-url-stuff uri v)))

(define default-stuffer
  (stuffer-chain
   serialize-stuffer
   is-url-too-big?
   (stuffer-chain
    gzip-stuffer 
    base64-stuffer)
   is-url-too-big?
   (md5-stuffer (md5-home))))

(define URL-KEY "c")

(define (do-url-stuff uri c)
  (insert-param uri URL-KEY (bytes->string/utf-8 c)))

(define (stuff-url stuffer uri c)
  (do-url-stuff
   uri
   ((stuffer-in stuffer) c)))

(define (stuffed-url? uri)
  (string? (extract-param uri URL-KEY)))

(define (unstuff-url stuffer uri)
  ((stuffer-out stuffer)
   (string->bytes/utf-8
    (extract-param uri URL-KEY))))

(provide/contract
 [struct stuffer
         ([in (any/c . -> . bytes?)]
          [out (bytes? . -> . any/c)])]
 [id-stuffer stuffer?]
 [serialize-stuffer stuffer?]
 [default-stuffer stuffer?])

;; OLD

(provide/contract
 [max-url-length (parameter/c number?)]
 [url-too-big? (url? . -> . boolean?)]
 [stuff-url (stuffer? url? serializable? . -> . url?)]
 [stuffed-url? (url? . -> . boolean?)]
 [unstuff-url (stuffer? url? . -> . serializable?)])

; http://www.boutell.com/newfaq/misc/urllength.html
(define max-url-length
  (make-parameter 2048))

(define (url-too-big? uri)
  ((string-length (url->string uri)) . > . (max-url-length)))
