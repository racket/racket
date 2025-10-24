#lang racket/base

(require file/gunzip
         openssl
         racket/contract/base
         racket/list
         racket/match
         racket/port
         racket/string
         (rename-in racket/tcp
                    [tcp-connect plain-tcp-connect]
                    [tcp-abandon-port plain-tcp-abandon-port])
         "platform-ssl.rkt")

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
    (raise
     (exn:fail:network
      "http-client: Connection ended early"
      (current-continuation-marks))))
  bs)

(define (regexp-member rx l)
  (ormap (λ (h) (regexp-match rx h)) l))

(define PIPE-SIZE 4096)

;; Core

(struct http-conn (host port port-usual? to from abandon-p
                        auto-reconnect? auto-reconnect-host auto-reconnect-ssl?) #:mutable)

(define (make-http-conn)
  (http-conn #f #f #f #f #f #f #f #f #f))

(define (http-conn-live? hc)
  (define to (http-conn-to hc))
  (define from (http-conn-from hc))
  (and to (not (port-closed? to))
       from (not (port-closed? from))
       #t))

(define (http-conn-liveable? hc)
  (or (http-conn-live? hc)
      (http-conn-auto-reconnect? hc)))

(define (http-conn-open! hc host-bs #:ssl? [ssl? #f] #:port [port (if ssl? 443 80)]
                         #:auto-reconnect? [auto-reconnect? #f])
  (http-conn-close! hc)
  (define host (->string host-bs))
  (define ssl-version (if (boolean? ssl?) 'auto ssl?))

  (define-values (from to)
    (cond [(list? ssl?)
           ;; At this point, we have a tunneled socket to the remote
           ;; host/port: we do not need to address it; ignore host-bs,
           ;; only use port for conn-port-usual?
           (match-define (list ssl-ctx?
                               (? input-port? t:from)
                               (? output-port? t:to)
                               abandon-p) ssl?)
           (set-http-conn-abandon-p! hc abandon-p)
           (set-http-conn-port-usual?! hc (or (and ssl-ctx? (= 443 port))
                                              (and (not ssl-ctx?) (= 80 port))))
           (values t:from t:to)]
          [ssl?
           (set-http-conn-port-usual?! hc (= 443 port))
           (cond
             [(osx-old-openssl?)
              ;; OpenSSL is either not available or too old; use
              ;; native OS X tools
              (set-http-conn-abandon-p! hc osx-ssl-abandon-port)
              (osx-ssl-connect host port ssl-version)]
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
  (set-http-conn-from! hc from)

  (set-http-conn-auto-reconnect?! hc auto-reconnect?)
  (set-http-conn-auto-reconnect-host! hc host-bs)
  (set-http-conn-auto-reconnect-ssl?! hc ssl?))

(define (http-conn-close! hc)
  (match-define (http-conn host port port-usual? to from abandon
                           auto-reconnect? auto-reconnect-host auto-reconnect-ssl?) hc)
  (set-http-conn-host! hc #f)
  (when to
    (close-output-port to)
    (set-http-conn-to! hc #f))
  (when from
    (close-input-port from)
    (set-http-conn-from! hc #f))
  ;; Doesn't seem necessary because on a reconnect, the same abandon
  ;; will be discovered.
  #;(set-http-conn-abandon-p! hc #f))

(define (http-conn-abandon! hc)
  (match-define (http-conn host port port-usual? to from abandon
                           auto-reconnect? auto-reconnect-host auto-reconnect-ssl?) hc)
  (when to
    (abandon to)
    (set-http-conn-to! hc #f)))

(define (http-conn-enliven! hc)
  (when (and (not (http-conn-live? hc)) (http-conn-auto-reconnect? hc))
    (http-conn-open! hc (http-conn-auto-reconnect-host hc)
                     #:ssl? (http-conn-auto-reconnect-ssl? hc)
                     #:port (http-conn-port hc)
                     #:auto-reconnect? (http-conn-auto-reconnect? hc))))

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
                         #:content-decode [decodes '(gzip deflate)]
                         #:data [data #f])
  (http-conn-enliven! hc)
  (match-define (http-conn host port port-usual? to from _
                           auto-reconnect? auto-reconnect-host auto-reconnect-ssl?) hc)
  (fprintf to "~a ~a HTTP/~a\r\n" method-bss url-bs version-bs)
  (unless (regexp-member #rx"^(?i:Host:)[\t ]*.+$" headers-bs)
    (fprintf to "Host: ~a\r\n"
             (if port-usual?
               host
               (format "~a:~a" host port))))
  (unless (regexp-member #rx"^(?i:User-Agent:)[\t ]*.+$" headers-bs)
    (fprintf to "User-Agent: Racket/~a (net/http-client)\r\n"
             (version)))
  (unless (or (empty? decodes)
              (regexp-member #rx"^(?i:Accept-Encoding:)[\t ]*.+$" headers-bs))
    (fprintf to "Accept-Encoding: ~a\r\n"
             (string-join (map symbol->string decodes) ",")))

  (define body (->bytes data))
  (cond [(procedure? body)
         (fprintf to "Transfer-Encoding: chunked\r\n")]
        [(and
          ;; Some hosts fail when given a Content-Length of 0. So, avoid
          ;; sending the header if there isn't any content and the
          ;; method doesn't expect a body.
          ;;
          ;; xref: https://github.com/racket/racket/pull/5200
          (or (expects-body? method-bss)
              (not (bytes=? body #"")))
          (not (regexp-member #rx"^(?i:Content-Length:)[\t ]*.+$" headers-bs)))
         (fprintf to "Content-Length: ~a\r\n" (bytes-length body))])
  (when close?
    (unless (regexp-member #rx"^(?i:Connection:)[\t ]*.+$" headers-bs)
      (fprintf to "Connection: close\r\n")))
  (for ([h (in-list headers-bs)])
    (fprintf to "~a\r\n" h))
  (fprintf to "\r\n")
  (cond [(procedure? body)
         (body (λ (data) (write-chunk to data)))
         (fprintf to "0\r\n\r\n")]
        [(not (bytes=? body #""))
         (display body to)])
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

(define (http-conn-response-port/length! hc n #:close? [close? #f])
  (make-conn-response-port
   hc close?
   (lambda (in out)
     (copy-bytes in out n))))

(define (http-conn-response-port/chunked! hc #:close? [close? #f])
  (make-conn-response-port
   hc close?
   (lambda (in out)
     (http-pipe-chunk in out))))

(define (http-pipe-chunk ip op)
  (define crlf-bytes
    (make-bytes 2))
  (let loop ([last-bytes #f])
    (define in-v (read-line ip eol-type))
    (cond
      [(eof-object? in-v)
       (void)]
      [else
       (define size-str
         (string-trim in-v))
       (define chunk-size
         (string->number size-str 16))
       (unless chunk-size
         (error 'http-pipe-chunk
                "Could not parse ~S as hexadecimal number"
                size-str))
       (define use-last-bytes?
         (and last-bytes (<= chunk-size (bytes-length last-bytes))))
       (cond
         [(zero? chunk-size)
          (void)]
         [else
          (define bs
            (cond
              [use-last-bytes?
               (read-bytes! last-bytes ip 0 chunk-size)
               last-bytes]
              [else
               (read-bytes chunk-size ip)]))
          (when (eof-object? bs)
            (error 'http-pipe-chunk "Unexpected EOF while reading chunk"))
          (read-bytes! crlf-bytes ip 0 2)
          (write-bytes bs op 0 chunk-size)
          (loop bs)])])))

;; The other end can hang up while we're reading the response, so we
;; have to ensure that reading from the port doesn't deadlock and that
;; the thread reading the port we return is notified of the exn.
(define (make-conn-response-port hc close? copy)
  (define-values (in out set-err!)
    (make-pipe/err PIPE-SIZE))
  (thread
   (lambda ()
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (set-err! e)
                        (http-conn-close! hc)
                        (close-output-port out))])
       (copy (http-conn-from hc) out)
       (when close? (http-conn-close! hc))
       (close-output-port out))))
  in)

;; Like make-pipe, but returns 3 values: the input port, the output port
;; and a procedure to signal that an error has ocurred while piping
;; data. When an error a signaled, in-progress and subsequent reads from
;; the input port raise that error.
(define (make-pipe/err size)
  (define-values (in out)
    (make-pipe size))
  (define err #f)
  (define (make-ready-or-err-evt)
    ;; When an error has already been signaled, raise immediately.
    (when err
      (raise err))
    ;; Otherwise wait for the port to make progress or be closed.
    ;; Assumes the user of make-pipe/err will close the output port
    ;; after signaling an error.
    (wrap-evt
     in
     (lambda (ip)
       (when err
         (raise err))
       ip)))
  (define (set-err! e)
    (set! err e))
  (define custom-in
    (make-input-port
     #;name
     'http-conn-response-port
     #;read-in
     (lambda (bs)
       (wrap-evt
        (make-ready-or-err-evt)
        (lambda (ip)
          (read-bytes-avail! bs ip))))
     #;peek
     (lambda (bs s progress)
       (wrap-evt
        (make-ready-or-err-evt)
        (lambda (ip)
          (peek-bytes-avail! bs s progress ip))))
     #;close
     (lambda ()
       (close-input-port in))
     #;get-progress-evt
     (lambda ()
       (port-progress-evt in))
     #;commit
     (lambda (amt progress evt)
       (port-commit-peeked amt progress evt in))
     #;get-location
     (lambda ()
       (port-next-location in))
     #;count-lines!
     (lambda ()
       (port-count-lines! in))
     #;init-position 1))
  (values custom-in out set-err!))

;; Derived

(define (http-conn-open host-bs #:ssl? [ssl? #f] #:port [port (if ssl? 443 80)]
                        #:auto-reconnect? [auto-reconnect? #f])
  (define hc (make-http-conn))
  (http-conn-open! hc host-bs #:ssl? ssl? #:port port #:auto-reconnect? auto-reconnect?)
  hc)

(define (http-conn-CONNECT-tunnel proxy-host proxy-port target-host target-port #:ssl? [ssl? #f])
  (define hc (http-conn-open proxy-host #:port proxy-port #:ssl? #f))
  (define connect-string (format "~a:~a" target-host target-port))
  (http-conn-send! hc #:method "CONNECT" connect-string #:headers
                   (list (format "Host: ~a" connect-string)
                         "Proxy-Connection: Keep-Alive"
                         "Connection: Keep-Alive"))

  (let ((tunnel-status (http-conn-status! hc))
        (tunnel-headers (http-conn-headers! hc)))
    (unless (regexp-match "^HTTP[^ ]* +2" tunnel-status)
      (error 'make-ports "HTTP CONNECT failed: ~a" tunnel-status)))

  ;; SSL secure the ports
  (match-define (http-conn _ _ _ t:to t:from _
                           auto-reconnect? auto-reconnect-host auto-reconnect-ssl?) hc)
  (cond [(not ssl?) ; it's just a tunnel... no ssl
         (define abandon-p (lambda (p) ((http-conn-abandon-p hc) p)))
         (values ssl? t:from t:to abandon-p)]

        [else ; ssl
          (define ssl-version (if (boolean? ssl?) 'auto ssl?))
          (set-http-conn-port-usual?! hc (= 443 target-port))
          ;; choose between win32 or non-win32 openssl here, then keep
          ;; code common afterwards
          (define-values (p->ssl-ps ssl-abndn-p)
            (if (or ssl-available? (not win32-ssl-available?))
              (values ports->ssl-ports ssl-abandon-port)
              (values ports->win32-ssl-ports win32-ssl-abandon-port)))

          (define clt-ctx
            (match ssl-version
                   [(? ssl-client-context? ctx) ctx]
                   [(? symbol? protocol) (ssl-make-client-context protocol)]))

          (define-values (r:from r:to) (p->ssl-ps t:from t:to
                                                  #:mode 'connect
                                                  #:context clt-ctx
                                                  #:close-original? #t
                                                  #:hostname target-host))

          ;; The user of the tunnel relies on ports->ssl-ports'
          ;; #:close-original? to close/abandon the underlying ports
          ;; of the tunnel itself. Therefore the abandon-p sent back
          ;; to caller is the ssl-abandon of the wrapped ports.
          (define abandon-p ssl-abndn-p)
          (values clt-ctx r:from r:to abandon-p)]))

(define (delete? method-bss)
  (or (equal? method-bss #"DELETE")
      (equal? method-bss "DELETE")
      (equal? method-bss 'DELETE)))

(define (get? method-bss)
  (or (equal? method-bss #"GET")
      (equal? method-bss "GET")
      (equal? method-bss 'GET)))

(define (head? method-bss)
  (or (equal? method-bss #"HEAD")
      (equal? method-bss "HEAD")
      (equal? method-bss 'HEAD)))

;; https://www.rfc-editor.org/rfc/rfc9110.html#name-get
;; https://www.rfc-editor.org/rfc/rfc9110.html#name-head
;; https://www.rfc-editor.org/rfc/rfc9110.html#name-delete
(define (expects-body? method-bss)
  (and (not (get? method-bss))
       (not (head? method-bss))
       (not (delete? method-bss))))

;; https://datatracker.ietf.org/doc/html/rfc2616#section-10.1
;; https://datatracker.ietf.org/doc/html/rfc2616#section-10.2.5
;; https://datatracker.ietf.org/doc/html/rfc2616#section-10.3.5
(define (no-content? status)
  (regexp-match? #rx#"^HTTP.... (1..|204|304) " status))

(define (http-conn-recv! hc
                         #:method [method-bss #"GET"]
                         #:content-decode [decodes '(gzip deflate)]
                         #:close? [iclose? #f])
  (http-conn-enliven! hc)
  (define status (http-conn-status! hc))
  (define headers (http-conn-headers! hc))
  (define close?
    (or iclose?
        (regexp-member #rx#"^(?i:Connection:[\t ]*close)$" headers)))
  (when close?
    (http-conn-abandon! hc))
  (define-values (raw-response-port wait-for-close?)
    (cond
      [(head? method-bss)
       (values (open-input-bytes #"") #f)]
      [(regexp-member #rx#"^(?i:Transfer-Encoding:[\t ]*chunked)$" headers)
       (values (http-conn-response-port/chunked! hc #:close? #t)
               #t)]
      [(ormap (λ (h)
                (match (regexp-match #rx#"^(?i:Content-Length:)[\t ]*(.+)$" h)
                  [#f #f]
                  [(list _ cl-bs)
                   (string->number
                    (bytes->string/utf-8 cl-bs))]))
              headers)
       =>
       (λ (count)
         (values (http-conn-response-port/length! hc count #:close? close?)
                 close?))]
      ;; Perform this check after the previous two in case the server
      ;; side returns an empty-but-chunked body, an explicit content
      ;; length of 0, or decides to return a body despite what the
      ;; spec says.
      [(no-content? status)
       (values (open-input-bytes #"") #f)]
      [else
       (values (http-conn-response-port/rest! hc) #t)]))
  (define decoded-response-port
    (let ([decode-response
           (λ (raw-response-port decode-function)
             (define-values (in out) (make-pipe PIPE-SIZE))
             (define decode-t
               (thread
                (λ ()
                  (decode-function raw-response-port out))))
             (thread
              (λ ()
                (thread-wait decode-t)
                (when wait-for-close?
                  ;; Wait for an EOF from the raw port before we send an
                  ;; output on the decoding pipe:
                  (copy-port raw-response-port (open-output-nowhere)))
                (close-output-port out)))
             in)])
      (cond
        [(head? method-bss) raw-response-port]
        [(and (memq 'gzip decodes)
              (regexp-member #rx#"^(?i:Content-Encoding:[\t ]*gzip)$" headers)
              (not (eof-object? (peek-byte raw-response-port))))
         (decode-response raw-response-port gunzip-through-ports)]
        [(and (memq 'deflate decodes)
              (regexp-member #rx#"^(?i:Content-Encoding:[\t ]*deflate)$" headers)
              (not (eof-object? (peek-byte raw-response-port))))
         (decode-response raw-response-port inflate)]
        [else
         raw-response-port])))
  (values status headers decoded-response-port))

(define (http-conn-sendrecv! hc url-bs
                             #:version [version-bs #"1.1"]
                             #:method [method-bss #"GET"]
                             #:headers [headers-bs empty]
                             #:data [data #f]
                             #:content-decode [decodes '(gzip deflate)]
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
                       #:content-decode [decodes '(gzip deflate)])
  (define hc (http-conn-open host-bs #:ssl? ssl? #:port port))
  (begin0 (http-conn-sendrecv! hc url-bs
                               #:version version-bs
                               #:method method-bss
                               #:headers headers-bs
                               #:data data
                               #:content-decode decodes
                               #:close? #t)
    (when (head? method-bss)
      (http-conn-close! hc))))

(define data-procedure/c
  (-> (-> (or/c bytes? string?) void?) any))

(define base-ssl?/c
  (or/c boolean? ssl-client-context? symbol?))

(define base-ssl?-tnl/c
  (or/c base-ssl?/c (list/c base-ssl?/c input-port? output-port? (-> port? void?))))

(provide
 data-procedure/c
 base-ssl?/c
 base-ssl?-tnl/c

 (contract-out
  [http-conn?
   (-> any/c
       boolean?)]
  [http-conn-live?
   (-> any/c
       boolean?)]
  [http-conn-liveable?
   (-> any/c
       boolean?)]
  [rename
   make-http-conn http-conn
   (-> http-conn?)]
  [http-conn-open!
   (->* (http-conn? (or/c bytes? string?))
        (#:ssl? base-ssl?-tnl/c
         #:port (between/c 1 65535)
         #:auto-reconnect? boolean?)
        void?)]
  [http-conn-close!
   (-> http-conn? void?)]
  [http-conn-abandon!
   (-> http-conn? void?)]
  [http-conn-enliven!
   (-> http-conn-liveable? void?)]
  [http-conn-send!
   (->*
    (http-conn-liveable? (or/c bytes? string?))
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
        (#:ssl? base-ssl?-tnl/c
         #:port (between/c 1 65535)
         #:auto-reconnect? boolean?)
        http-conn?)]
  [http-conn-CONNECT-tunnel
    (->* ((or/c bytes? string?)
          (between/c 1 65535)
          (or/c bytes? string?)
          (between/c 1 65535))
         (#:ssl? base-ssl?/c)
         (values base-ssl?/c input-port? output-port? (-> port? void?)))]
  [http-conn-recv!
   (->* (http-conn-liveable?)
        (#:content-decode (listof symbol?)
         #:method (or/c bytes? string? symbol?)
         #:close? boolean?)
        (values bytes? (listof bytes?) input-port?))]
  [http-conn-sendrecv!
   (->* (http-conn-liveable? (or/c bytes? string?))
        (#:version (or/c bytes? string?)
         #:method (or/c bytes? string? symbol?)
         #:headers (listof (or/c bytes? string?))
         #:data (or/c false/c bytes? string? data-procedure/c)
         #:content-decode (listof symbol?)
         #:close? boolean?)
        (values bytes? (listof bytes?) input-port?))]
  [http-sendrecv
   (->* ((or/c bytes? string?) (or/c bytes? string?))
        (#:ssl? base-ssl?-tnl/c
         #:port (between/c 1 65535)
         #:version (or/c bytes? string?)
         #:method (or/c bytes? string? symbol?)
         #:headers (listof (or/c bytes? string?))
         #:data (or/c false/c bytes? string? data-procedure/c)
         #:content-decode (listof symbol?))
        (values bytes? (listof bytes?) input-port?))]))
