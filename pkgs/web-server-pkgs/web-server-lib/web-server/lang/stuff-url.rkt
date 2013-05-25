#lang racket/base
(require racket/contract
         racket/list
         net/url
         racket/serialize
         web-server/private/servlet
         web-server/stuffers/stuffer
         web-server/stuffers/serialize
         web-server/stuffers/gzip
         web-server/stuffers/base64
         web-server/stuffers/hash
         web-server/http
         web-server/private/url-param)

(define (is-url-too-big? v)
  (define uri
    (request-uri 
     (execution-context-request
      (current-execution-context))))
  (> (string-length
      (url->string
       (insert-in-uri uri v)))
     ; http://www.boutell.com/newfaq/misc/urllength.html
     2048))

(define (make-default-stuffer home)
  (stuffer-chain
   serialize-stuffer
   is-url-too-big?
   (stuffer-chain
    gzip-stuffer 
    base64-stuffer)
   is-url-too-big?
   (md5-stuffer home)))

(define default-stuffer
  (make-default-stuffer
   (build-path (find-system-path 'home-dir) ".urls")))

(define URL-KEY "c")

(define (insert-in-uri uri c)
  (insert-param uri URL-KEY (bytes->string/utf-8 c)))

(define serialize-rx #rx"serialize: contract violation\n  expected: serializable\\?\n  given: (.*)")

(define (stuff-url stuffer uri c)
  (with-handlers
   ([(lambda (x)
       (and (exn:fail? x)
            (regexp-match serialize-rx
                          (exn-message x))))
     (lambda (x)
       (define non
         (second
          (regexp-match serialize-rx
                       (exn-message x))))
       (error 'stuff-url 
              "Cannot stuff ~e into a URL because it contains non-serializable pieces. Convert ~a to a serializable struct"
              c non))])
   (insert-in-uri
    uri ((stuffer-in stuffer) c))))

(define (stuffed-url? uri)
  (string? (extract-param uri URL-KEY)))

(define (unstuff-url stuffer uri)
  ((stuffer-out stuffer)
   (string->bytes/utf-8
    (extract-param uri URL-KEY))))

(provide/contract
 [default-stuffer (stuffer/c serializable? bytes?)]
 [make-default-stuffer (path-string? . -> . (stuffer/c serializable? bytes?))]
 [is-url-too-big? (bytes? . -> . boolean?)]
 [stuff-url ((stuffer/c serializable? bytes?) url? serializable? . -> . url?)]
 [stuffed-url? (url? . -> . boolean?)]
 [unstuff-url ((stuffer/c serializable? bytes?) url? . -> . serializable?)])
