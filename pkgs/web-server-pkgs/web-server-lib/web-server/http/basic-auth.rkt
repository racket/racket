#lang racket/base
(require racket/contract
         racket/match
         net/base64
         web-server/http/request-structs)

(define (request->basic-credentials req)
  (define headers (request-headers/raw req))
  (match (headers-assq* #"Authorization" headers)
    [#f #f]
    [(struct header (_ basic-credentials))
     (cond
       [(and (basic? basic-credentials)
             (regexp-match #rx"([^:]*):(.*)"
                           (base64-decode (subbytes basic-credentials 6 (bytes-length basic-credentials)))))
        => (lambda (user-pass)
             (cons (cadr user-pass) (caddr user-pass)))]
       [else #f])]))

;; basic?: bytes -> (or/c (listof bytes) #f)
;; does the second part of the authorization header start with #"Basic "
(define basic?
  (let ([rx (byte-regexp #"^Basic .*")])
    (lambda (a) (regexp-match rx a))))

(define (make-basic-auth-header realm)
  (make-header #"WWW-Authenticate" (string->bytes/utf-8 (format "Basic realm=\"~a\"" realm))))

(provide/contract
 [make-basic-auth-header (string? . -> . header?)]
 [request->basic-credentials (request? . -> . (or/c false/c (cons/c bytes? bytes?)))])
