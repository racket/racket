#lang racket/base
(require unstable/bytes
         net/base64
         net/cookie
         racket/match
         racket/file
         racket/contract
         web-server/http
         web-server/stuffers/hmac-sha1)

(define (substring* s st en)
  (substring s st (+ (string-length s) en)))

(define (mac key v)
  (substring*
   (bytes->string/utf-8
    (base64-encode (HMAC-SHA1 key (write/bytes v))))
   0 -3))

(define (make-secret-salt/file secret-salt-path)
  (unless (file-exists? secret-salt-path)
    (with-output-to-file secret-salt-path
      (λ ()
        (for ([i (in-range 128)])
          (write-byte (random 256))))))
  (file->bytes secret-salt-path))

(define (id-cookie? name c)
  (and (client-cookie? c)
       (string=? (client-cookie-name c) name)))

(define (make-id-cookie name key data)
  (define authored (current-seconds))
  (define digest
    (mac key (list authored data)))
  (make-cookie name
               (format "~a&~a&~a"
                       digest authored data)))

(define (valid-id-cookie? name key timeout c)
  (and (id-cookie? name c)
       (with-handlers ([exn:fail? (lambda (x) #f)])
         (match (client-cookie-value c)
           [(regexp #rx"^(.+)&(.+)&(.*)$" (list _ digest authored-s data))
            (define authored (string->number authored-s))
            (define re-digest (mac key (list authored data)))
            (and (string=? digest re-digest)
                 (<= authored timeout)
                 data)]
           [cv
            #f]))))

(define (request-id-cookie
         name
         key
         #:timeout [timeout +inf.0]
         req)
  (define cookies (request-cookies req))
  (for/or ([c (in-list cookies)])
    (valid-id-cookie? name key timeout c)))

(define (logout-id-cookie name)
  (make-cookie name "invalid format"))

(provide
 (contract-out
  [make-secret-salt/file
   (-> path-string?
       bytes?)]
  [logout-id-cookie
   (-> cookie-name? cookie?)]
  [request-id-cookie
   (->* (cookie-name? bytes? request?)
        (#:timeout number?)
        (or/c false/c cookie-value?))]
  [make-id-cookie
   (-> cookie-name? bytes? cookie-value?
       cookie?)]))
