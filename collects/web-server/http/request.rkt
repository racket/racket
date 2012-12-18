#lang racket/base
(require racket/contract
         racket/match
         racket/list
         racket/promise
         net/url
         net/uri-codec
         unstable/contract
         web-server/private/util
         web-server/private/connection-manager
         web-server/http/request-structs)

(define read-request/c
  (connection? 
   tcp-listen-port?
   (input-port? . -> . (values string? string?))
   . -> .
   (values request? boolean?)))

(provide/contract
 [parse-bindings (-> bytes? (listof binding?))]
 [read-headers (-> input-port? (listof header?))]
 [rename make-ext:read-request make-read-request
         (->* () (#:connection-close? boolean?) read-request/c)]
 [rename ext:read-request read-request
         read-request/c])

;; **************************************************
;; read-request: connection number (input-port -> string string) -> request boolean?
;; read the request line, and the headers, determine if the connection should
;; be closed after servicing the request and build a request structure
(define ((make-read-request 
          #:connection-close? [connection-close? #f])
         conn host-port port-addresses)
  (define ip 
    (connection-i-port conn))
  (define-values (method uri major minor)
    (read-request-line ip))
  (define initial-headers 
    (read-headers ip))
  (match (headers-assq* #"Content-Length" initial-headers)
    [(struct header (f v))
     ;; Give it one second per byte (with 5 second minimum... a bit
     ;; arbitrary)
     (adjust-connection-timeout!
      conn (max 5 (string->number (bytes->string/utf-8 v))))]
    [#f
     (void)])
  (define-values (data-ip headers)
    (complete-request ip initial-headers))
  (define-values (host-ip client-ip)
    (port-addresses ip))
  (define-values (bindings/raw-promise raw-post-data)
    (read-bindings&post-data/raw data-ip method uri headers))
  (values
   (make-request method uri headers bindings/raw-promise raw-post-data
                 host-ip host-port client-ip)
   (or connection-close?
       (close-connection? headers major minor
                          client-ip host-ip))))

;; If the headers says it uses chunked transfer encoding, then decode
;; it
(require racket/stxparam
         (for-syntax racket/base))
(define-syntax-parameter break 
  (λ (stx) 
    (raise-syntax-error 'break "Used outside forever" stx)))
(define-syntax-rule (forever e ...)
  (let/ec this-break
    (let loop ()
      (syntax-parameterize ([break (make-rename-transformer #'this-break)])
        (begin e ...))
      (loop))))
(define (hex-string->number s)
  (string->number s 16))
(define (complete-request real-ip initial-headers)
  (match (headers-assq* #"Transfer-Encoding" initial-headers)
    [(struct header (f #"chunked"))
     (define-values (decoded-ip decode-op) (make-pipe))
     (define total-size 0)
     (forever
      (define size-line (read-line real-ip 'any))
      (match-define (cons size-in-hex _) (regexp-split #rx";" size-line))
      (define size-in-bytes (hex-string->number size-in-hex))
      (set! total-size (+ total-size size-in-bytes))
      (when (zero? size-in-bytes)
        (break))
      (define data-bytes (read-bytes size-in-bytes real-ip))
      (write-bytes data-bytes decode-op)
      ;; Ignore CRLF
      (read-line real-ip 'any))
     (define more-headers
       (list* (header #"Content-Length" 
                      (string->bytes/utf-8 (number->string total-size)))
              (read-headers real-ip)))
     (close-output-port decode-op)
     (values decoded-ip (append initial-headers more-headers))]
    [_
     (values real-ip initial-headers)]))

(define (make-ext:read-request 
         #:connection-close? [connection-close? #f])
  (define read-request 
    (make-read-request #:connection-close? connection-close?))
  (define (ext:read-request conn host-port port-addresses)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (kill-connection! conn)
                       (raise exn))])
      (read-request conn host-port port-addresses)))
  ext:read-request)

(define ext:read-request (make-ext:read-request #:connection-close? #f))

;; **************************************************
;; close-connection?

; close-connection? : (listof (cons symbol bytes)) number number string string -> boolean
;; determine if this connection should be closed after serving the
;; response
(define close-connection? 
  (let ([rx (byte-regexp #"[cC][lL][oO][sS][eE]")])
    (lambda (headers major minor client-ip host-ip)
      (or (< major 1)
          (and (= major 1) (= minor 0))
          (match (headers-assq* #"Connection" headers)
            [(struct header (f v))
             (and (regexp-match rx v)
                  #t)]
            [#f
             #f])
          (msie-from-local-machine? headers client-ip host-ip)))))

;; msie-from-local-machine? : table str str -> bool

;; to work around a bug in MSIE for documents < 265 bytes when
;; connecting from the local machine.  The server could pad the
;; response as MSIIS does, but closing the connection works, too.  We
;; do not check for version numbers since IE 6 under windows is 5.2
;; under macosX
(define msie-from-local-machine?
  (let ([rx (byte-regexp #"MSIE")])
    (lambda (headers client-ip host-ip)
      (and (string=? host-ip client-ip)
           (match
               (or (headers-assq* #"HTTP_USER_AGENT" headers)
                   (headers-assq* #"User-Agent" headers))
             [(struct header (f v))
              (and (regexp-match rx v)
                   #t)]
             [#f
              #f])))))

;; **************************************************
;; read-request-line  
(define match-method
  (let ([rx (byte-regexp #"^([^ ]+) (.+) HTTP/([0-9]+)\\.([0-9]+)$")])
    (lambda (a) (regexp-match rx a))))

; read-request-line : iport -> bytes url number number
; to read in the first line of an http request, AKA the "request line"
; effect: in case of errors, complain [MF: where] and close the ports
(define (read-request-line ip)
  (define line (read-bytes-line ip 'any))
  (if (eof-object? line)
      (network-error 'read-request "http input closed abruptly")
      (cond
        [(match-method line)
         => (match-lambda
              [(list _ method url major minor)
               (values method
                       (string->url (bytes->string/utf-8 url))
                       (string->number (bytes->string/utf-8 major))
                       (string->number (bytes->string/utf-8 minor)))])]
        [else (network-error 'read-request "malformed request ~a" line)])))

;; **************************************************
;; read-headers  
(define match-colon
  (let ([rx (byte-regexp (bytes-append #"^([^:]*):[ " (bytes 9) #"]*(.*)"))])
    (lambda (a) (regexp-match rx a))))

; read-headers : iport -> (listof header?)
(define (read-headers in)
  (let read-header ()
    (define l (read-bytes-line in 'any))
    (cond
      [(eof-object? l) null]
      [(zero? (bytes-length l)) null]
      [(match-colon l) 
       => (match-lambda
            [(list _ field value)
             (list* (make-header field (read-one-head in value))
                    (read-header))])]
      [else (network-error 'read-headers "malformed header: ~e" l)])))

; read-one-head : iport bytes -> bytes
(define (read-one-head in rhs)
  (match (peek-byte in)
    [(or 32 9) ;(or (eq? c #\space) (eq? c #\tab))
     ; (read-bytes-line in 'any) can't return eof
     ; because we just checked with peek-char
     ; Spidey: FLOW
     (read-one-head in (bytes-append rhs (read-bytes-line in 'any)))]
    [_ rhs]))

;; **************************************************
;; read-bindings
(define INPUT-BUFFER-SIZE 4096)
(define (read-to-eof in)
  (define b (read-bytes INPUT-BUFFER-SIZE in))
  (if (eof-object? b)
      empty
      (list* b (read-to-eof in))))

(define FILE-FORM-REGEXP (byte-regexp #"multipart/form-data; *boundary=(.*)"))

;; read-bindings&post-data/raw: input-port symbol url (listof header?) -> (values (or/c (listof binding?) string?) (or/c bytes? false/c?))
(define (read-bindings&post-data/raw in meth uri headers)
  (define bindings-GET
   (delay
     (filter-map
      (match-lambda
       [(list-rest k v)
        (if (and (symbol? k) (string? v))
          (make-binding:form (string->bytes/utf-8 (symbol->string k))
                             (string->bytes/utf-8 v))
          #f)])
      (url-query uri))))
  (cond
    [(bytes-ci=? #"GET" meth)
     (values bindings-GET #f)]
    [(bytes-ci=? #"POST" meth)
     (define content-type (headers-assq* #"Content-Type" headers))
     (cond
       [(and content-type 
             (regexp-match FILE-FORM-REGEXP (header-value content-type)))
        => (match-lambda
            [(list _ content-boundary)
             ;; XXX This can't be delay because it reads from the
             ;;     port, which would otherwise be closed.  I think
             ;;     this is reasonable because the Content-Type
             ;;     said it would have this format
             (define bs
               (map (match-lambda
                     [(struct mime-part (headers contents))
                      (define rhs
                        (header-value
                         (headers-assq* #"Content-Disposition" headers)))
                      (match* 
                          ((regexp-match #"filename=(\"([^\"]*)\"|([^ ;]*))" rhs)
                           (regexp-match #"[^e]name=(\"([^\"]*)\"|([^ ;]*))" rhs))
                        [(#f #f)
                         (network-error 
                          'reading-bindings 
                          "Couldn't extract form field name for file upload")]
                        [(#f (list _ _ f0 f1))
                         (make-binding:form (or f0 f1)
                                            (apply bytes-append contents))]
                        [((list _ _ f00 f01) (list _ _ f10 f11))
                         (make-binding:file (or f10 f11)
                                            (or f00 f01)
                                            headers
                                            (apply bytes-append contents))])])
                    (read-mime-multipart content-boundary in)))
             (values
              (delay (append (force bindings-GET) bs))
              #f)])]
       [else        
        (match (headers-assq* #"Content-Length" headers)
          [(struct header (_ value))
           (cond
             [(string->number (bytes->string/utf-8 value))
              => (lambda (len)
                   (let ([raw-bytes (read-bytes len in)])
                     (values (delay (append (parse-bindings raw-bytes) (force bindings-GET))) raw-bytes)))]
             [else 
              (network-error
               'read-bindings
               "Post request contained a non-numeric content-length")])]
          [#f
           (values (delay empty) #f)])])]
    [meth
     (define content-type (headers-assq* #"Content-Type" headers))
     (match (headers-assq* #"Content-Length" headers)
       [(struct header (_ value))
        (cond [(string->number (bytes->string/utf-8 value))
               => (lambda (len)
                    (let ([raw-bytes (read-bytes len in)])
                      (values (delay empty) raw-bytes)))]
              [else
               (network-error 
                'read-bindings
                "Non-GET/POST request contained a non-numeric content-length")])]
       [#f
        (values (delay empty) #f)])]))

;; parse-bindings : bytes? -> (listof binding?)
(define (parse-bindings raw)
  (define len (bytes-length raw))
  (let loop ([start 0])
    (let find= ([key-end start])
      (if (>= key-end len)
          empty
          (if (eq? (bytes-ref raw key-end) (char->integer #\=))
              (let find-amp ([amp-end (add1 key-end)])
                (if (or (= amp-end len) (eq? (bytes-ref raw amp-end) (char->integer #\&)))
                    (list* (make-binding:form
                            (string->bytes/utf-8
                             (form-urlencoded-decode
                              (bytes->string/utf-8
                               (subbytes raw start key-end))))
                            (string->bytes/utf-8
                             (form-urlencoded-decode
                              (bytes->string/utf-8
                               (subbytes raw (add1 key-end) amp-end)))))
                           (loop (add1 amp-end)))
                    (find-amp (add1 amp-end))))
              (find= (add1 key-end)))))))

;; **************************************************
;; read-mime-multipart

; mime-part : (listof header?) * (listof bytes?)
(define-struct mime-part (headers contents))
(define CR-NL #"\r\n")
(define (construct-mime-part headers body)
  (make-mime-part
   headers
   (match body
     [(list)
      (list)]
     [(list-rest fst rst)
      (list* fst
             (foldr (lambda (byt acc)
                      (list* CR-NL byt acc))
                    empty
                    rst))])))

; read-mime-multipart : bytes iport -> (listof part)
(define (read-mime-multipart boundary in)
  (define boundary-len (bytes-length boundary))
  (define start-boundary (bytes-append #"--" boundary))
  (define end-boundary (bytes-append start-boundary #"--"))
  (let skip-preamble ()
    (define line (read-bytes-line in 'return-linefeed))
    (cond
      [(eof-object? line)
       (network-error 'read-mime-multipart "Port prematurely closed.")]
      [(bytes=? line start-boundary)
       (let read-parts ()
         (define headers (read-headers in))
         (let read-mime-part-body 
           ([more-k (lambda (contents)
                      (list* (construct-mime-part
                              headers contents)
                             (read-parts)))]
            [end-k (lambda (contents)
                     (list (construct-mime-part
                            headers contents)))])
           (define line (read-bytes-line in 'return-linefeed))
           (cond
             [(eof-object? line)
              (network-error 'read-mime-multipart "Port prematurely closed.")]
             [(bytes=? line start-boundary)
              (more-k empty)]
             [(bytes=? line end-boundary)
              (end-k empty)]
             [else 
              (read-mime-part-body
               (lambda (x) (more-k (list* line x)))
               (lambda (x) (end-k (list* line x))))])))]
      [(bytes=? line end-boundary) null]
      [else (skip-preamble)])))
