#lang racket/base
(require racket/contract/base
         racket/match
         racket/list
         racket/string
         racket/port
         (rename-in racket/tcp
                    [tcp-connect plain-tcp-connect]
                    [tcp-abandon-port plain-tcp-abandon-port])
         openssl
         "win32-ssl.rkt"
         file/gunzip)

(define tolerant? #t)
(define eol-type
  (if tolerant?
    'any
    'return-linefeed))

;; Lib

(define (->string bs)
  (if (bytes? bs)
    (bytes->string/utf-8 bs)
    bs))

(define (->bytes str)
  (cond
    [(string? str)
     (string->bytes/utf-8 str)]
    [(not str)
     #""]
    [else
     str]))

(define (read-bytes-line/not-eof ip kind)
  (define bs (read-bytes-line ip kind))
  (when (eof-object? bs)
    (error 'http-client "Connection ended early"))
  bs)

(define (regexp-member rx l)
  (ormap (λ (h) (regexp-match rx h)) l))

(define PIPE-SIZE 4096)

;; Core

(struct http-conn (host port port-usual? to from abandon-p) #:mutable)

(define (make-http-conn)
  (http-conn #f #f #f #f #f #f))

(define (http-conn-live? hc)
  (and (http-conn-to hc)
       (http-conn-from hc)
       #t))

(define (http-conn-open! hc host-bs #:ssl? [ssl? #f] #:port [port (if ssl? 443 80)])
  (http-conn-close! hc)
  (define host (->string host-bs))
  (define ssl-version (if (boolean? ssl?) 'auto ssl?))

  (define-values (from to)
    (cond [ssl?
           (set-http-conn-port-usual?! hc (= 443 port))
           (cond
             [(or ssl-available? (not win32-ssl-available?))
              (set-http-conn-abandon-p! hc ssl-abandon-port)              
              (ssl-connect host port ssl-version)]
             [else
              (set-http-conn-abandon-p! hc win32-ssl-abandon-port)
              (win32-ssl-connect host port ssl-version)])]
          [else
           (set-http-conn-abandon-p! hc plain-tcp-abandon-port)
           (set-http-conn-port-usual?! hc (= 80 port))
           (plain-tcp-connect host port)]))

  (set-http-conn-host! hc host)
  (set-http-conn-port! hc port)

  ;; (define-values (log-i log-o) (make-pipe))
  ;; (thread (λ () (copy-port log-i to (current-error-port))))

  (set-http-conn-to! hc to)
  (set-http-conn-from! hc from))

(define (http-conn-close! hc)
  (match-define (http-conn host port port-usual? to from abandon) hc)
  (set-http-conn-host! hc #f)
  (when to
    (close-output-port to)
    (set-http-conn-to! hc #f))
  (when from
    (close-input-port from)
    (set-http-conn-from! hc #f))
  (set-http-conn-abandon-p! hc #f))

(define (http-conn-abandon! hc)
  (match-define (http-conn host port port-usual? to from abandon) hc)
  (when to
    (abandon to)
    (set-http-conn-to! hc #f)))

(define (write-chunk out data)
  (let ([bytes (->bytes data)])
    (define len (bytes-length bytes))
    (unless (zero? len)
      (fprintf out "~x\r\n~a\r\n" len bytes))))

(define (http-conn-send! hc url-bs
                         #:version [version-bs #"1.1"]
                         #:method [method-bss #"GET"]
                         #:close? [close? #f]
                         #:headers [headers-bs empty]
                         #:content-decode [decodes '(gzip)]
                         #:data [data #f])
  (match-define (http-conn host port port-usual? to from _) hc)
  (fprintf to "~a ~a HTTP/~a\r\n" method-bss url-bs version-bs)
  (unless (regexp-member #rx"^(?i:Host:) +.+$" headers-bs)
    (fprintf to "Host: ~a\r\n" 
             (if port-usual?
               host
               (format "~a:~a" host port))))
  (unless (regexp-member #rx"^(?i:User-Agent:) +.+$" headers-bs)
    (fprintf to "User-Agent: Racket/~a (net/http-client)\r\n" 
             (version)))
  (unless (or (not (memq 'gzip decodes))
              (regexp-member #rx"^(?i:Accept-Encoding:) +.+$" headers-bs))
    (fprintf to "Accept-Encoding: gzip\r\n"))
  (define body (->bytes data))
  (cond [(procedure? body)
         (fprintf to "Transfer-Encoding: chunked\r\n")]
        [(and body
              (not (regexp-member #rx"^(?i:Content-Length:) +.+$" headers-bs)))
         (fprintf to "Content-Length: ~a\r\n" (bytes-length body))])
  (when close?
    (unless (regexp-member #rx"^(?i:Connection:) +.+$" headers-bs)
      (fprintf to "Connection: close\r\n")))
  (for ([h (in-list headers-bs)])
    (fprintf to "~a\r\n" h))
  (fprintf to "\r\n")
  (cond [(procedure? body)
         (body (λ (data) (write-chunk to data)))
         (fprintf to "0\r\n\r\n")]
        [body (display body to)])
  (flush-output to))

(define (http-conn-status! hc)
  (read-bytes-line/not-eof (http-conn-from hc) eol-type))

(define (http-conn-headers! hc)
  (define top (read-bytes-line/not-eof (http-conn-from hc) eol-type))  
  (if (bytes=? top #"")
    empty
    (cons top (http-conn-headers! hc))))

(define BUFFER-SIZE 1024)
(define (copy-bytes in out count)
  (define buffer (make-bytes BUFFER-SIZE))
  (let loop ([count count])
    (when (positive? count)
      (define r 
        (read-bytes-avail! buffer in 0
                           (if (< count BUFFER-SIZE)
                             count
                             BUFFER-SIZE)))
      (unless (eof-object? r)
        (write-bytes buffer out 0 r)
        (loop (- count r))))))

(define (http-conn-response-port/rest! hc)
  (http-conn-response-port/length! hc +inf.0 #:close? #t))

(define (http-conn-response-port/length! hc count #:close? [close? #f])
  (define-values (in out) (make-pipe PIPE-SIZE))
  (thread
   (λ ()
     (copy-bytes (http-conn-from hc) out count)
     (when close?
       (http-conn-close! hc))
     (close-output-port out)))
  in)

(define (http-conn-response-port/chunked! hc #:close? [close? #f])
  (define (http-pipe-chunk ip op)
    (define crlf-bytes (make-bytes 2))
    (let loop ([last-bytes #f])
      (define size-str (string-trim (read-line ip eol-type)))
      (define chunk-size (string->number size-str 16))
      (unless chunk-size
        (error 'http-conn-response/chunked 
               "Could not parse ~S as hexadecimal number"
               size-str))
      (define use-last-bytes?
        (and last-bytes (<= chunk-size (bytes-length last-bytes))))
      (if (zero? chunk-size)
        (begin (flush-output op)
               (close-output-port op))
        (let* ([bs (if use-last-bytes?
                     (begin
                       (read-bytes! last-bytes ip 0 chunk-size)
                       last-bytes)
                     (read-bytes chunk-size ip))]
               [crlf (read-bytes! crlf-bytes ip 0 2)])
          (write-bytes bs op 0 chunk-size)
          (loop bs)))))

  (define-values (in out) (make-pipe PIPE-SIZE))
  (define chunk-t
    (thread
     (λ ()
       (http-pipe-chunk (http-conn-from hc) out))))
  (thread
   (λ ()
     (thread-wait chunk-t)
     (when close?
       (http-conn-close! hc))
     (close-output-port out)))
  in)

;; Derived

(define (http-conn-open host-bs #:ssl? [ssl? #f] #:port [port (if ssl? 443 80)])
  (define hc (make-http-conn))
  (http-conn-open! hc host-bs #:ssl? ssl? #:port port)
  hc)

(define (http-conn-recv! hc
                         #:method [method-bss #"GET"]
                         #:content-decode [decodes '(gzip)]
                         #:close? [iclose? #f])
  (define status (http-conn-status! hc))
  (define headers (http-conn-headers! hc))
  (define close?
    (or iclose?
        (regexp-member #rx#"^(?i:Connection: +close)$" headers)))
  (when close?
    (http-conn-abandon! hc))
  (define head?
    (or (equal? method-bss #"HEAD")
        (equal? method-bss "HEAD")
        (equal? method-bss 'HEAD)))
  (define raw-response-port
    (cond
      [head? (open-input-bytes #"")]
      [(regexp-member #rx#"^(?i:Transfer-Encoding: +chunked)$" headers)
       (http-conn-response-port/chunked! hc #:close? #t)]
      [(ormap (λ (h)
                (match (regexp-match #rx#"^(?i:Content-Length:) +(.+)$" h)
                  [#f #f]
                  [(list _ cl-bs)
                   (string->number
                    (bytes->string/utf-8 cl-bs))]))
              headers)
       =>
       (λ (count)
         (http-conn-response-port/length! hc count #:close? close?))]
      [else
       (http-conn-response-port/rest! hc)]))
  (define decoded-response-port
    (cond
      [head? raw-response-port]
      [(and (memq 'gzip decodes)
            (regexp-member #rx#"^(?i:Content-Encoding: +gzip)$" headers)
            (not (eof-object? (peek-byte raw-response-port))))
       (define-values (in out) (make-pipe PIPE-SIZE))
       (define gunzip-t
         (thread
          (λ ()
            (gunzip-through-ports raw-response-port out))))
       (thread 
        (λ ()
          (thread-wait gunzip-t)
          (close-output-port out)))
       in]
      [else 
       raw-response-port]))
  (values status headers decoded-response-port))

(define (http-conn-sendrecv! hc url-bs
                             #:version [version-bs #"1.1"]
                             #:method [method-bss #"GET"]
                             #:headers [headers-bs empty]
                             #:data [data #f]
                             #:content-decode [decodes '(gzip)]
                             #:close? [close? #f])
  (http-conn-send! hc url-bs
                   #:version version-bs
                   #:method method-bss
                   #:close? close?
                   #:headers headers-bs
                   #:content-decode decodes
                   #:data data)
  (http-conn-recv! hc 
                   #:method method-bss
                   #:content-decode decodes
                   #:close? close?))

(define (http-sendrecv host-bs url-bs
                       #:ssl? [ssl? #f]
                       #:port [port (if ssl? 443 80)]
                       #:version [version-bs #"1.1"]
                       #:method [method-bss #"GET"]
                       #:headers [headers-bs empty]
                       #:data [data #f]
                       #:content-decode [decodes '(gzip)])
  (define hc (http-conn-open host-bs #:ssl? ssl? #:port port))
  (http-conn-sendrecv! hc url-bs
                       #:version version-bs
                       #:method method-bss
                       #:headers headers-bs
                       #:data data
                       #:content-decode decodes
                       #:close? #t))

(define data-procedure/c
  (-> (-> (or/c bytes? string?) void?) any))

(provide
 data-procedure/c
 (contract-out
  [http-conn?
   (-> any/c
       boolean?)]
  [http-conn-live?
   (-> any/c
       boolean?)]
  [rename
   make-http-conn http-conn
   (-> http-conn?)]
  [http-conn-open!
   (->* (http-conn? (or/c bytes? string?))
        (#:ssl? (or/c boolean? ssl-client-context? symbol?)
                #:port (between/c 1 65535))
        void?)]
  [http-conn-close!
   (-> http-conn? void?)]
  [http-conn-abandon!
   (-> http-conn? void?)]
  [http-conn-send!
   (->*
    (http-conn-live? (or/c bytes? string?))
    (#:version (or/c bytes? string?)
               #:method (or/c bytes? string? symbol?)
               #:close? boolean?
               #:headers (listof (or/c bytes? string?))
               #:content-decode (listof symbol?)                           
               #:data (or/c false/c bytes? string? data-procedure/c))
    void)]
  ;; Derived
  [http-conn-open
   (->* ((or/c bytes? string?))
        (#:ssl? (or/c boolean? ssl-client-context? symbol?)
                #:port (between/c 1 65535))
        http-conn?)]
  [http-conn-recv!
   (->* (http-conn-live?)
        (#:content-decode (listof symbol?)
                          #:method (or/c bytes? string? symbol?)
                          #:close? boolean?)
        (values bytes? (listof bytes?) input-port?))]
  [http-conn-sendrecv!
   (->* (http-conn-live? (or/c bytes? string?))
        (#:version (or/c bytes? string?)
                   #:method (or/c bytes? string? symbol?)
                   #:headers (listof (or/c bytes? string?))
                   #:data (or/c false/c bytes? string? data-procedure/c)
                   #:content-decode (listof symbol?) 
                   #:close? boolean?)
        (values bytes? (listof bytes?) input-port?))]
  [http-sendrecv
   (->* ((or/c bytes? string?) (or/c bytes? string?))
        (#:ssl? (or/c boolean? ssl-client-context? symbol?)
                #:port (between/c 1 65535)
                #:version (or/c bytes? string?)
                #:method (or/c bytes? string? symbol?)
                #:headers (listof (or/c bytes? string?))
                #:data (or/c false/c bytes? string? data-procedure/c)
                #:content-decode (listof symbol?))
        (values bytes? (listof bytes?) input-port?))]))
