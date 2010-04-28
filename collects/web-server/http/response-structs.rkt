#lang racket
(require racket
         xml
         web-server/private/xexpr
         web-server/http/request-structs)

(define TEXT/HTML-MIME-TYPE #"text/html; charset=utf-8")

(define-struct response/basic (code message seconds mime headers))
(define-struct (response/full response/basic) (body))
(define-struct (response/incremental response/basic) (generator))

(define response/c
  (or/c response/basic?
        (cons/c bytes? (listof (or/c string? bytes?)))
        pretty-xexpr/c))

;; response/full->size: response/full -> number
(define (response/full->size resp)
  (apply + (map bytes-length (response/full-body resp))))

(define (normalize-response close? resp)
  (cond
    [(response/full? resp)
     (make-response/full 
      (response/basic-code resp)
      (response/basic-message resp)
      (response/basic-seconds resp)
      (response/basic-mime resp)
      (list* (make-header #"Content-Length" (string->bytes/utf-8 (number->string (response/full->size resp))))
             (response/basic-headers resp))
      (response/full-body resp))]
    [(response/incremental? resp)
     (if close?
         resp
         (make-response/incremental 
          (response/basic-code resp)
          (response/basic-message resp)
          (response/basic-seconds resp)
          (response/basic-mime resp)
          (list* (make-header #"Transfer-Encoding" #"chunked")
                 (response/basic-headers resp))
          (response/incremental-generator resp)))]
    [(response/basic? resp)
     (normalize-response
      close?
      (make-response/full 
       (response/basic-code resp)
       (response/basic-message resp)
       (response/basic-seconds resp)
       (response/basic-mime resp)
       (response/basic-headers resp)
       empty))]
    [(and (list? resp)
          (not (empty? resp))
          (bytes? (first resp))
          (andmap (lambda (i) (or (string? i)
                                  (bytes? i)))
                  (rest resp)))
     (normalize-response
      close?
      (make-response/full 
       200 #"Okay" (current-seconds) (car resp) empty
       (map (lambda (bs)
              (if (string? bs)
                  (string->bytes/utf-8 bs)
                  bs))
            (rest resp))))]
    [else
     (normalize-response
      close?
      (make-xexpr-response resp))]))

(define (make-xexpr-response
         xexpr
         #:code [code 200] 
         #:message [message #"Okay"]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
         #:headers [hdrs empty])
  (make-response/full 
   code message seconds mime-type hdrs
   (list (string->bytes/utf-8 (xexpr->string xexpr)))))

(provide/contract
 [struct response/basic
         ([code number?]
          [message bytes?]
          [seconds number?]
          [mime bytes?]
          [headers (listof header?)])]            
 [struct (response/full response/basic)
         ([code number?]
          [message bytes?]
          [seconds number?]
          [mime bytes?]
          [headers (listof header?)]
          [body (listof bytes?)])]
 [struct (response/incremental response/basic)
         ([code number?]
          [message bytes?]
          [seconds number?]
          [mime bytes?]
          [headers (listof header?)]
          [generator ((() () #:rest (listof bytes?) . ->* . any) . -> . any)])]
 [response/c contract?]
 [make-xexpr-response 
  ((pretty-xexpr/c)
   (#:code number? #:message bytes? #:seconds number? #:mime-type bytes? #:headers (listof header?))
   . ->* . response/full?)]
 [normalize-response (boolean? response/c . -> . (or/c response/full? response/incremental?))]
 [TEXT/HTML-MIME-TYPE bytes?])
