#lang racket/base

(require racket/contract
         (only-in racket/bytes bytes-join)
         "common.rkt")

(provide (contract-out (struct cookie
                         ([name       (and/c string? cookie-name?)]
                          [value      (and/c string? cookie-value?)]
                          [expires    (or/c date? #f)]
                          [max-age    (or/c (and/c integer? positive?) #f)]
                          [domain     (or/c domain-value? #f)]
                          [path       (or/c path/extension-value? #f)]
                          [secure?    boolean?]
                          [http-only? boolean?]
                          [extension  (or/c path/extension-value? #f)])
                         #:omit-constructor)
                       [make-cookie
                        (->* (cookie-name? cookie-value?)
                             (#:expires    (or/c date? #f)
                              #:max-age    (or/c (and/c integer? positive?) #f)
                              #:domain     (or/c domain-value? #f)
                              #:path       (or/c path/extension-value? #f)
                              #:secure?    boolean?
                              #:http-only? boolean?
                              #:extension  (or/c path/extension-value? #f))
                             cookie?)]
                       
                       [cookie->set-cookie-header (-> cookie? bytes?)]
                       [clear-cookie-header
                        (->* (cookie-name?)
                             (#:domain     (or/c domain-value? #f)
                              #:path       (or/c path/extension-value? #f))
                             bytes?)]
                       [cookie->string (-> cookie? string?)]
                       
                       #:forall X
                       [cookie-header->alist
                        (case-> (-> bytes? (listof (cons/c bytes? bytes?)))
                                (-> bytes? (-> bytes? X)
                                    (listof (cons/c X X))))]
                       ))

(require racket/serialize ; for serializable cookie structs
         srfi/19          ; for date handling
         (only-in racket/string
                  string-join string-split)
         racket/match
         )


(serializable-struct cookie
  [name value expires max-age domain path secure? http-only? extension]
  #:transparent)

(define (make-cookie name value
                     #:expires    [expires #f]
                     #:max-age    [max-age #f]
                     #:domain     [domain #f]
                     #:path       [path #f]
                     #:secure?    [secure? #f]
                     #:http-only? [http-only? #f]
                     #:extension  [extension #f])
  (cookie name value expires max-age domain path secure? http-only? extension))

;; cookie -> String
;; produce a Set-Cookie header suitable for sending to a client
(define (cookie->set-cookie-header c)
  (string->bytes/utf-8 (cookie->string c)))

;; produce a Set-Cookie header suitable for telling the client to
;; clear a cookie
(define clear-cookie-expiration-seconds 1420070400) ; midnight UTC on 1/1/15
(define (clear-cookie-header name #:path [path #f] #:domain [domain #f])
  (cookie->set-cookie-header
   (make-cookie name ""
                #:expires (seconds->date clear-cookie-expiration-seconds #f)
                #:path path
                #:domain domain)))

;; bytes? [(bytes? -> A)] -> (AList A A)
;; Given the value from a Cookie: header, produces an alist of name/value
;; mappings. If there is no value, the empty byte-string (or whatever
;; decode produces for #"") is used as the value.
;; XXX Would it be worthwhile to give the option to pass separate decoders
;; for keys and values?
(define (cookie-header->alist header [decode (lambda (x) x)])
  (define header-pairs (regexp-split #"; " header))
  (reverse
   (for/fold ([cookies '()]) ([bs header-pairs]
                              #:unless (or (bytes=? bs #"")
                                           (= (bytes-ref bs 0) #x3d)))
     (match (regexp-split #"=" bs)
       [(list) cookies]
       [(list (? cookie-name? key) (? cookie-value? val))
        (cons (cons (decode key) (decode val)) cookies)]
       [(list-rest (? cookie-name? key) val-parts)
        #:when (andmap cookie-value? val-parts)
        (cons (cons (decode key) (decode (bytes-join val-parts #"=")))
              cookies)]
       [_ cookies]))))

(define (cookie->string c)
  (define (maybe-format fmt val) (and val (format fmt val)))
  (match c
    [(cookie name value expires max-age domain path secure? http-only? extension)
     (string-join
      (filter values
              (list (format "~a=~a" name value)
                    (and expires
                         (format "Expires=~a"
                                 (date->string expires
                                               rfc1123:date-template)))
                    (maybe-format "Max-Age=~a" max-age)
                    (maybe-format "Domain=~a" domain)
                    (maybe-format "Path=~a" path)
                    (and secure? "Secure")
                    (and http-only? "HttpOnly")
                    extension))
      "; ")]
    [_ (error 'cookie->string "expected a cookie; received: ~a" c)]))


#|
From RFC6265:

   HTTP applications have historically allowed three different formats
   for the representation of date/time stamps:

      Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
      Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
      Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format

   The first format is preferred as an Internet standard and represents
   a fixed-length subset of that defined by RFC 1123 [8] (an update to
   RFC 822 [9]). The second format is in common use, but is based on the
   obsolete RFC 850 [12] date format and lacks a four-digit year.
   HTTP/1.1 clients and servers that parse the date value MUST accept
   all three formats (for compatibility with HTTP/1.0), though they MUST
   only generate the RFC 1123 format for representing HTTP-date values
   in header fields...

      Note: Recipients of date values are encouraged to be robust in
      accepting date values that may have been sent by non-HTTP
      applications, as is sometimes the case when retrieving or posting
      messages via proxies/gateways to SMTP or NNTP.

   All HTTP date/time stamps MUST be represented in Greenwich Mean Time
   (GMT), without exception. For the purposes of HTTP, GMT is exactly
   equal to UTC (Coordinated Universal Time). This is indicated in the
   first two formats by the inclusion of "GMT" as the three-letter
   abbreviation for time zone, and MUST be assumed when reading the
   asctime format. HTTP-date is case sensitive and MUST NOT include
   additional LWS beyond that specifically included as SP in the
   grammar.
|#
(define rfc1123:date-template "~a, ~d ~b ~Y ~H:~M:~S GMT")
(define rfc850:date-template "~A, ~d-~b-~y ~H:~M:~S GMT")
(define asctime:date-template "~a ~b ~e ~H:~M:~S ~Y")

