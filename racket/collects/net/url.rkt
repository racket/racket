#lang racket/base
(require racket/port
         racket/string
         racket/contract/base
         racket/list
         racket/match
         (prefix-in hc: "http-client.rkt")
         (only-in "url-connect.rkt" current-https-protocol)
         "uri-codec.rkt"
         "url-string.rkt"
         (only-in "url-exception.rkt" make-url-exception))

;; To do:
;;   Handle HTTP/file errors.
;;   Not throw away MIME headers.
;;     Determine file type.

(define-logger net/url)

;; ----------------------------------------------------------------------

;; Input ports have two statuses:
;;   "impure" = they have text waiting
;;   "pure" = the MIME headers have been read

(define current-proxy-servers
  (make-parameter null
                  (lambda (v)
                    (unless (and (list? v)
                                 (andmap (lambda (v)
                                           (and (list? v)
                                                (= 3 (length v))
                                                (equal? (car v) "http")
                                                (string? (car v))
                                                (exact-integer? (caddr v))
                                                (<= 1 (caddr v) 65535)))
                                         v))
                      (raise-type-error
                       'current-proxy-servers
                       "list of list of scheme, string, and exact integer in [1,65535]"
                       v))
                    (map (lambda (v)
                           (list (string->immutable-string (car v))
                                 (string->immutable-string (cadr v))
                                 (caddr v)))
                         v))))

(define (url-error fmt . args)
  (raise (make-url-exception
          (apply format fmt
                 (map (lambda (arg) (if (url? arg) (url->string arg) arg))
                      args))
          (current-continuation-marks))))

;; url->default-port : url -> num
(define (url->default-port url)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme) 80]
          [(string=? scheme "http") 80]
          [(string=? scheme "https") 443]
          [else (url-error "URL scheme ~s not supported" scheme)])))

;; make-ports : url -> hc
(define (make-ports url proxy)
  (let ([port-number (if proxy
                       (caddr proxy)
                       (or (url-port url) (url->default-port url)))]
        [host (if proxy (cadr proxy) (url-host url))])
    (hc:http-conn-open host
                       #:port port-number
                       #:ssl? (if (equal? "https" (url-scheme url))
                                (current-https-protocol)
                                #f))))

;; http://getpost-impure-port : bool x url x union (str, #f) x list (str)
;;                               -> hc
(define (http://getpost-impure-port get? url post-data strings
                                    make-ports 1.1?)
  (define proxy (assoc (url-scheme url) (current-proxy-servers)))
  (define hc (make-ports url proxy))
  (define access-string
    (url->string
     (if proxy
       url
       ;; RFCs 1945 and 2616 say:
       ;;   Note that the absolute path cannot be empty; if none is present in
       ;;   the original URI, it must be given as "/" (the server root).
       (let-values ([(abs? path)
                     (if (null? (url-path url))
                       (values #t (list (make-path/param "" '())))
                       (values (url-path-absolute? url) (url-path url)))])
         (make-url #f #f #f #f abs? path (url-query url) (url-fragment url))))))

  (hc:http-conn-send! hc access-string
                      #:method (if get? "GET" "POST")
                      #:headers strings
                      #:content-decode '()
                      #:data post-data)
  hc)

;; file://get-pure-port : url -> in-port
(define (file://get-pure-port url)
  (open-input-file (file://->path url)))

(define (schemeless-url url)
  (url-error "Missing protocol (usually \"http:\") at the beginning of URL: ~a" url))

;; getpost-impure-port : bool x url x list (str) -> in-port
(define (getpost-impure-port get? url post-data strings)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme)
           (schemeless-url url)]
          [(or (string=? scheme "http") (string=? scheme "https"))
           (define hc (http://getpost-impure-port get? url post-data strings make-ports #f))
           (http-conn-impure-port hc)]
          [(string=? scheme "file")
           (url-error "There are no impure file: ports")]
          [else (url-error "Scheme ~a unsupported" scheme)])))

(define (http-conn-impure-port hc)
  (define-values (in out) (make-pipe 4096))
  (define-values (status headers response-port)
    (hc:http-conn-recv! hc #:close? #t #:content-decode '()))
  (fprintf out "~a\r\n" status)
  (for ([h (in-list headers)])
    (fprintf out "~a\r\n" h))
  (fprintf out "\r\n")
  (thread
   (λ ()
     (copy-port response-port out)
     (close-output-port out)))
  in)

;; get-impure-port : url [x list (str)] -> in-port
(define (get-impure-port url [strings '()])
  (getpost-impure-port #t url #f strings))

;; post-impure-port : url x bytes [x list (str)] -> in-port
(define (post-impure-port url post-data [strings '()])
  (getpost-impure-port #f url post-data strings))

;; getpost-pure-port : bool x url x list (str) -> in-port
(define (getpost-pure-port get? url post-data strings redirections)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme)
           (schemeless-url url)]
          [(or (string=? scheme "http")
               (string=? scheme "https"))
           (cond
             [(or (not get?)
                  ;; do not follow redirections for POST
                  (zero? redirections))
              (define-values (status headers response-port)
                (hc:http-conn-recv!
                 (http://getpost-impure-port
                  get? url post-data strings
                  make-ports #f)
                  #:content-decode '()
                 #:close? #t))
              response-port]
             [else
              (define-values (port header)
                (get-pure-port/headers url strings #:redirections redirections))
              port])]
          [(string=? scheme "file")
           (file://get-pure-port url)]
          [else (url-error "Scheme ~a unsupported" scheme)])))

(define (make-http-connection)
  (hc:http-conn))

(define (http-connection-close hc)
  (hc:http-conn-close! hc))

(define (get-pure-port/headers url [strings '()]
                               #:redirections [redirections 0]
                               #:status? [status? #f]
                               #:connection [conn #f])
  (let redirection-loop ([redirections redirections] [url url] [use-conn conn])
    (define hc
      (http://getpost-impure-port #t url #f strings
                                  (if (and use-conn
                                           (hc:http-conn-live? use-conn))
                                    (lambda (url proxy)
                                      (log-net/url-debug "reusing connection")
                                      use-conn)
                                    make-ports)
                                  (and conn #t)))
    (define-values (status headers response-port)
      (hc:http-conn-recv! hc #:close? (not conn) #:content-decode '()))

    (define new-url
      (ormap (λ (h)
               (match (regexp-match #rx#"^Location: (.*)$" h)
                 [#f #f]
                 [(list _ m1b)
                  (define m1 (bytes->string/utf-8 m1b))
                  (with-handlers ((exn:fail? (λ (x) #f)))
                    (define next-url (string->url m1))
                    (make-url
                     (or (url-scheme next-url) (url-scheme url))
                     (or (url-user next-url) (url-user url))
                     (or (url-host next-url) (url-host url))
                     (or (url-port next-url) (url-port url))
                     (url-path-absolute? next-url)
                     (url-path next-url)
                     (url-query next-url)
                     (url-fragment next-url)))]))
             headers))
    (define redirection-status-line?
      (regexp-match #rx#"^HTTP/[0-9]+[.][0-9]+ 3[0-9][0-9]" status))
    (cond
      [(and redirection-status-line? new-url (not (zero? redirections)))
       (log-net/url-info "redirection: ~a" (url->string new-url))
       (redirection-loop (- redirections 1) new-url #f)]
      [else
       (values response-port
               (apply string-append
                      (map (λ (x) (format "~a\r\n" x))
                           (if status?
                             (cons status headers)
                             headers))))])))

;; get-pure-port : url [x list (str)] -> in-port
(define (get-pure-port url [strings '()] #:redirections [redirections 0])
  (getpost-pure-port #t url #f strings redirections))

;; post-pure-port : url bytes [x list (str)] -> in-port
(define (post-pure-port url post-data [strings '()])
  (getpost-pure-port #f url post-data strings 0))

;; display-pure-port : in-port -> ()
(define (display-pure-port server->client)
  (copy-port server->client (current-output-port))
  (close-input-port server->client))

;; call/input-url : url x (url -> in-port) x (in-port -> T)
;;                  [x list (str)] -> T
(define call/input-url
  (let ([handle-port
         (lambda (server->client handler)
           (dynamic-wind (lambda () 'do-nothing)
               (lambda () (handler server->client))
               (lambda () (close-input-port server->client))))])
    (case-lambda
      [(url getter handler)
       (handle-port (getter url) handler)]
      [(url getter handler params)
       (handle-port (getter url params) handler)])))

;; purify-port : in-port -> header-string
(define (purify-port port)
  (let ([m (regexp-match-peek-positions
            #rx"^HTTP/.*?(?:\r\n\r\n|\n\n|\r\r)" port)])
    (if m (read-string (cdar m) port) "")))

;; purify-http-port : in-port -> in-port
(define (purify-http-port in-port)
  (purify-port in-port)
  in-port)

;; delete-pure-port : url [x list (str)] -> in-port
(define (delete-pure-port url [strings '()])
  (method-pure-port 'delete url #f strings))

;; delete-impure-port : url [x list (str)] -> in-port
(define (delete-impure-port url [strings '()])
  (method-impure-port 'delete url #f strings))

;; head-pure-port : url [x list (str)] -> in-port
(define (head-pure-port url [strings '()])
  (method-pure-port 'head url #f strings))

;; head-impure-port : url [x list (str)] -> in-port
(define (head-impure-port url [strings '()])
  (method-impure-port 'head url #f strings))

;; put-pure-port : url bytes [x list (str)] -> in-port
(define (put-pure-port url put-data [strings '()])
  (method-pure-port 'put url put-data strings))

;; put-impure-port : url x bytes [x list (str)] -> in-port
(define (put-impure-port url put-data [strings '()])
  (method-impure-port 'put url put-data strings))

;; options-pure-port : url [x list (str)] -> in-port
(define (options-pure-port url [strings '()])
  (method-pure-port 'options url #f strings))

;; options-impure-port : url [x list (str)] -> in-port
(define (options-impure-port url [strings '()])
  (method-impure-port 'options url #f strings))

;; method-impure-port : symbol x url x list (str) -> in-port
(define (method-impure-port method url data strings)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme)
           (schemeless-url url)]
          [(or (string=? scheme "http") (string=? scheme "https"))
           (http://method-impure-port method url data strings)]
          [(string=? scheme "file")
           (url-error "There are no impure file: ports")]
          [else (url-error "Scheme ~a unsupported" scheme)])))

;; method-pure-port : symbol x url x list (str) -> in-port
(define (method-pure-port method url data strings)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme)
           (schemeless-url url)]
          [(or (string=? scheme "http") (string=? scheme "https"))
           (let ([port (http://method-impure-port
                        method url data strings)])
             (purify-http-port port))]
          [(string=? scheme "file")
           (file://get-pure-port url)]
          [else (url-error "Scheme ~a unsupported" scheme)])))

;; http://metod-impure-port : symbol x url x union (str, #f) x list (str) -> in-port
(define (http://method-impure-port method url data strings)
  (let* ([method (case method
                   [(get) "GET"] [(post) "POST"] [(head) "HEAD"]
                   [(put) "PUT"] [(delete) "DELETE"] [(options) "OPTIONS"] 
                   [else (url-error "unsupported method: ~a" method)])]
         [proxy (assoc (url-scheme url) (current-proxy-servers))]
         [hc (make-ports url proxy)]
         [access-string (url->string
                         (if proxy
                           url
                           (make-url #f #f #f #f
                                     (url-path-absolute? url)
                                     (url-path url)
                                     (url-query url)
                                     (url-fragment url))))])
    (hc:http-conn-send! hc access-string
                        #:method method
                        #:headers strings
                        #:content-decode '()
                        #:data data)
    (http-conn-impure-port hc)))

(provide (all-from-out "url-string.rkt"))

(provide/contract
 (get-pure-port (->* (url?) ((listof string?) #:redirections exact-nonnegative-integer?) input-port?))
 (get-impure-port (->* (url?) ((listof string?)) input-port?))
 (post-pure-port (->* (url? (or/c false/c bytes?)) ((listof string?)) input-port?))
 (post-impure-port (->* (url? bytes?) ((listof string?)) input-port?))
 (head-pure-port (->* (url?) ((listof string?)) input-port?))
 (head-impure-port (->* (url?) ((listof string?)) input-port?))
 (delete-pure-port (->* (url?) ((listof string?)) input-port?))
 (delete-impure-port (->* (url?) ((listof string?)) input-port?))
 (put-pure-port (->* (url? (or/c false/c bytes?)) ((listof string?)) input-port?))
 (put-impure-port (->* (url? bytes?) ((listof string?)) input-port?))
 (options-pure-port (->* (url?) ((listof string?)) input-port?))
 (options-impure-port (->* (url?) ((listof string?)) input-port?))
 (display-pure-port (input-port? . -> . void?))
 (purify-port (input-port? . -> . string?))
 (get-pure-port/headers (->* (url?)
                             ((listof string?)
                              #:redirections exact-nonnegative-integer?
                              #:status? boolean?
                              #:connection (or/c #f hc:http-conn?))
                             (values input-port? string?)))
 (rename hc:http-conn? http-connection? (any/c . -> . boolean?))
 (make-http-connection (-> hc:http-conn?))
 (http-connection-close (hc:http-conn? . -> . void?))
 (call/input-url (case-> (-> url?
                             (-> url? input-port?)
                             (-> input-port? any)
                             any)
                         (-> url?
                             (-> url? (listof string?) input-port?)
                             (-> input-port? any)
                             (listof string?)
                             any)))
 (current-proxy-servers
  (parameter/c (or/c false/c (listof (list/c string? string? number?))))))

(define (http-sendrecv/url u
                           #:method [method-bss #"GET"]
                           #:headers [headers-bs empty]
                           #:data [data #f]
                           #:content-decode [decodes '(gzip)])
  (unless (member (url-scheme u) '(#f "http" "https"))
    (error 'http-sendrecv/url "URL scheme ~e not supported" (url-scheme u)))
  (define ssl?
    (equal? (url-scheme u) "https"))
  (define port
    (or (url-port u)
        (if ssl?
          443
          80)))
  (unless (url-host u)
    (error 'http-sendrecv/url "Host required: ~e" u))
  (hc:http-sendrecv
   (url-host u)
   (url->string
    (make-url #f #f #f #f
              (url-path-absolute? u)
              (url-path u)
              (url-query u)
              (url-fragment u)))
   #:ssl?
   (if (equal? "https" (url-scheme u))
     (current-https-protocol)
     #f)
   #:port port
   #:method method-bss
   #:headers headers-bs
   #:data data
   #:content-decode decodes))

(provide
 (contract-out
  [http-sendrecv/url
   (->* (url?)
        (#:method (or/c bytes? string? symbol?)
                  #:headers (listof (or/c bytes? string?))
                  #:data (or/c false/c bytes? string? hc:data-procedure/c)
                  #:content-decode (listof symbol?))
        (values bytes? (listof bytes?) input-port?))]))
