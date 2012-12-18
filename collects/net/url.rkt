#lang racket/base
(require racket/port racket/string racket/contract/base
         "url-connect.rkt"
         "url-structs.rkt"
         "uri-codec.rkt")

;; To do:
;;   Handle HTTP/file errors.
;;   Not throw away MIME headers.
;;     Determine file type.

;; ----------------------------------------------------------------------

;; Input ports have two statuses:
;;   "impure" = they have text waiting
;;   "pure" = the MIME headers have been read

(define-struct (url-exception exn:fail) ())
(define (-url-exception? x)
  (or (url-exception? x)
      
      ;; two of the errors that string->url can raise are
      ;; now contract violations instead of url-expcetion
      ;; structs. since only the url-exception? predicate
      ;; was exported, we just add this in to the predicate
      ;; to preserve backwards compatibility
      (and (exn:fail:contract? x)
           (regexp-match? #rx"^string->url:" (exn-message x)))))

(define file-url-path-convention-type (make-parameter (system-path-convention-type)))

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

(define (url->string url)
  (let ([scheme (url-scheme url)]
        [user   (url-user url)]
        [host   (url-host url)]
        [port   (url-port url)]
        [path   (url-path url)]
        [query  (url-query url)]
        [fragment (url-fragment url)]
        [sa string-append])
    (when (and (equal? scheme "file")
               (not (url-path-absolute? url)))
      (raise-mismatch-error 'url->string
                            "cannot convert relative file URL to a string: "
                            url))
    (sa (if scheme (sa scheme ":") "")
        (if (or user host port)
          (sa "//"
              (if user (sa (uri-userinfo-encode user) "@") "")
              (if host host "")
              (if port (sa ":" (number->string port)) "")
              ;; There used to be a "/" here, but that causes an
              ;; extra leading slash -- wonder why it ever worked!
              )
          (if (equal? "file" scheme) ; always need "//" for "file" URLs
            "//"
            ""))
        (combine-path-strings (url-path-absolute? url) path)
        ;; (if query (sa "?" (uri-encode query)) "")
        (if (null? query) "" (sa "?" (alist->form-urlencoded query)))
        (if fragment (sa "#" (uri-encode* fragment)) ""))))

;; url->default-port : url -> num
(define (url->default-port url)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme) 80]
          [(string=? scheme "http") 80]
          [(string=? scheme "https") 443]
          [else (url-error "URL scheme ~s not supported" scheme)])))

;; make-ports : url -> in-port x out-port
(define (make-ports url proxy)
  (let ([port-number (if proxy
                       (caddr proxy)
                       (or (url-port url) (url->default-port url)))]
        [host (if proxy (cadr proxy) (url-host url))])
    (parameterize ([current-connect-scheme (url-scheme url)])
      (tcp-connect host port-number))))

;; http://getpost-impure-port : bool x url x union (str, #f) x list (str) -> in-port
(define (http://getpost-impure-port get? url post-data strings)
  (define proxy (assoc (url-scheme url) (current-proxy-servers)))
  (define-values (server->client client->server) (make-ports url proxy))
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
  (define (println . xs)
    (for-each (lambda (x) (display x client->server)) xs)
    (display "\r\n" client->server))
  (println (if get? "GET " "POST ") access-string " HTTP/1.0")
  (println "Host: " (url-host url)
           (let ([p (url-port url)]) (if p (format ":~a" p) "")))
  (when post-data (println "Content-Length: " (bytes-length post-data)))
  (for-each println strings)
  (println)
  (when post-data (display post-data client->server))
  (flush-output client->server)
  (tcp-abandon-port client->server)
  server->client)

(define (file://->path url [kind (system-path-convention-type)])
  (let ([strs (map path/param-path (url-path url))]
        [string->path-element/same
         (lambda (e)
           (if (symbol? e)
             e
             (if (string=? e "")
               'same
               (bytes->path-element (string->bytes/locale e) kind))))]
        [string->path/win (lambda (s)
                            (bytes->path (string->bytes/utf-8 s) 'windows))])
    (if (and (url-path-absolute? url)
             (eq? 'windows kind))
      ;; If initial path is "", then build UNC path.
      (cond
        [(not (url-path-absolute? url))
         (apply build-path (map string->path-element/same strs))]
        [(and ((length strs) . >= . 3)
              (equal? (car strs) ""))
         (apply build-path
                (string->path/win
                 (string-append "\\\\" (cadr strs) "\\" (caddr strs) "\\"))
                (map string->path-element/same (cdddr strs)))]
        [(pair? strs)
         (apply build-path (string->path/win (car strs))
                (map string->path-element/same (cdr strs)))]
        [else (error 'file://->path "no path elements: ~e" url)])
      (let ([elems (map string->path-element/same strs)])
        (if (url-path-absolute? url)
          (apply build-path (bytes->path #"/" 'unix) elems)
          (apply build-path elems))))))

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
           (http://getpost-impure-port get? url post-data strings)]
          [(string=? scheme "file")
           (url-error "There are no impure file: ports")]
          [else (url-error "Scheme ~a unsupported" scheme)])))

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
              (let ([port (http://getpost-impure-port
                           get? url post-data strings)])
                (purify-http-port port))]
             [else
              (define-values (port header) 
                (get-pure-port/headers url strings #:redirections redirections))
              port])]
          [(string=? scheme "file")
           (file://get-pure-port url)]
          [else (url-error "Scheme ~a unsupported" scheme)])))

(define (get-pure-port/headers url [strings '()] 
                               #:redirections [redirections 0]
                               #:status? [status? #f])
  (let redirection-loop ([redirections redirections] [url url])
    (define ip
      (http://getpost-impure-port #t url #f strings))
    (define status (read-line ip 'return-linefeed))
    (define-values (new-url chunked? headers)
      (let loop ([new-url #f] [chunked? #f] [headers '()])
        (define line (read-line ip 'return-linefeed))
        (when (eof-object? line)
          (error 'getpost-pure-port 
                 "connection ended before headers ended (when trying to follow a redirection)"))
        (cond
          [(equal? line "") (values new-url chunked? headers)]
          [(equal? chunked-header-line line) (loop new-url #t (cons line headers))]
          [(regexp-match #rx"^Location: (.*)$" line)
           =>
           (λ (m) 
             (define m1 (list-ref m 1))
             (define next-url 
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
                  (url-fragment next-url))))
             (loop (or next-url new-url) chunked? (cons line headers)))]
          [else (loop new-url chunked? (cons line headers))])))
    (define redirection-status-line?
      (regexp-match #rx"^HTTP/[0-9]+[.][0-9]+ 3[0-9][0-9]" status))
    (cond
      [(and redirection-status-line? new-url (not (zero? redirections))) 
       (close-input-port ip)
       (redirection-loop (- redirections 1) new-url)]
      [else
       (define-values (in-pipe out-pipe) (make-pipe))
       (thread
        (λ ()
          (http-pipe-data chunked? ip out-pipe)
          (close-input-port ip)))
       (values in-pipe
               (apply string-append (map (λ (x) (string-append x "\r\n"))
                                         (if status?
                                           (cons status (reverse headers))
                                           (reverse headers)))))])))

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

;; transliteration of code in rfc 3986, section 5.2.2
(define (combine-url/relative Base string)
  (let ([R (string->url string)]
        [T (make-url #f #f #f #f #f '() '() #f)])
    (if (url-scheme R)
      (begin
        (set-url-scheme! T (url-scheme R))
        (set-url-user! T (url-user R))  ;; authority
        (set-url-host! T (url-host R))  ;; authority
        (set-url-port! T (url-port R))  ;; authority
        (set-url-path-absolute?! T (url-path-absolute? R))
        (set-url-path! T (remove-dot-segments (url-path R)))
        (set-url-query! T (url-query R)))
      (begin
        (if (url-host R)  ;; => authority is defined
          (begin
            (set-url-user! T (url-user R))  ;; authority
            (set-url-host! T (url-host R))  ;; authority
            (set-url-port! T (url-port R))  ;; authority
            (set-url-path-absolute?! T (url-path-absolute? R))
            (set-url-path! T (remove-dot-segments (url-path R)))
            (set-url-query! T (url-query R)))
          (begin
            (if (null? (url-path R)) ;; => R has empty path
              (begin
                (set-url-path-absolute?! T (url-path-absolute? Base))
                (set-url-path! T (url-path Base))
                (if (not (null? (url-query R)))
                  (set-url-query! T (url-query R))
                  (set-url-query! T (url-query Base))))
              (begin
                (cond
                  [(url-path-absolute? R)
                   (set-url-path-absolute?! T #t)
                   (set-url-path! T (remove-dot-segments (url-path R)))]
                  [(and (null? (url-path Base))
                        (url-host Base))
                   (set-url-path-absolute?! T #t)
                   (set-url-path! T (remove-dot-segments (url-path R)))]
                  [else
                   (set-url-path-absolute?! T (url-path-absolute? Base))
                   (set-url-path! T (remove-dot-segments
                                     (append (all-but-last (url-path Base))
                                             (url-path R))))])
                (set-url-query! T (url-query R))))
            (set-url-user! T (url-user Base))   ;; authority
            (set-url-host! T (url-host Base))   ;; authority
            (set-url-port! T (url-port Base)))) ;; authority
        (set-url-scheme! T (url-scheme Base))))
    (set-url-fragment! T (url-fragment R))
    T))

(define (all-but-last lst)
  (cond [(null? lst) null]
        [(null? (cdr lst)) null]
        [else (cons (car lst) (all-but-last (cdr lst)))]))

;; cribbed from 5.2.4 in rfc 3986
;; the strange [*] cases implicitly change urls
;; with paths segments "." and ".." at the end
;; into "./" and "../" respectively
(define (remove-dot-segments path)
  (let loop ([path path] [result '()])
    (if (null? path)
      (reverse result)
      (let ([fst (path/param-path (car path))]
            [rst (cdr path)])
        (loop rst
              (cond
                [(and (eq? fst 'same) (null? rst))
                 (cons (make-path/param "" '()) result)] ; [*]
                [(eq? fst 'same)
                 result]
                [(and (eq? fst 'up) (null? rst) (not (null? result)))
                 (cons (make-path/param "" '()) (cdr result))] ; [*]
                [(and (eq? fst 'up) (not (null? result)))
                 (cdr result)]
                [(and (eq? fst 'up) (null? result))
                 ;; when we go up too far, just drop the "up"s.
                 result]
                [else
                 (cons (car path) result)]))))))

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
;; returns a new port, closes the old one when done pumping
(define (purify-http-port in-port)
  (define-values (in-pipe out-pipe) (make-pipe))
  (thread
   (λ ()
     (define status (http-read-status in-port))
     (define chunked? (http-read-headers in-port))
     (http-pipe-data chunked? in-port out-pipe)
     (close-input-port in-port)))
  in-pipe)

(define (http-read-status ip)
  (read-line ip 'return-linefeed))

(define (http-read-headers ip)
  (define l (read-line ip 'return-linefeed))
  (when (eof-object? l)
    (error 'purify-http-port "Connection ended before headers ended"))
  (if (string=? l "")
      #f
      (if (string=? l chunked-header-line)
          (begin (http-read-headers ip)
                 #t)
          (http-read-headers ip))))

(define chunked-header-line "Transfer-Encoding: chunked")

(define (http-pipe-data chunked? ip op)
  (if chunked?
      (http-pipe-chunk ip op)
      (begin
        (copy-port ip op)
        (flush-output op)
        (close-output-port op))))

(define (http-pipe-chunk ip op)
  (define crlf-bytes (make-bytes 2))
  (let loop ([last-bytes #f])
    (define size-str (read-line ip 'return-linefeed))
    (define chunk-size (string->number size-str 16))
    (unless chunk-size
      (error 'http-pipe-chunk "Could not parse ~S as hexadecimal number" size-str))
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

(define character-set-size 256)

;; netscape/string->url : str -> url
(define (netscape/string->url string)
  (let ([url (string->url string)])
    (cond [(url-scheme url) url]
          [(string=? string "")
           (url-error "Can't resolve empty string as URL")]
          [else (set-url-scheme! url
                  (if (char=? (string-ref string 0) #\/) "file" "http"))
                url])))

;; URL parsing regexp
;; this is following the regexp in Appendix B of rfc 3986, except for using
;; `*' instead of `+' for the scheme part (it is checked later anyway, and
;; we don't want to parse it as a path element), and the user@host:port is
;; parsed here.
(define url-regexp
  (regexp (string-append
           "^"
           "(?:"              ; / scheme-colon-opt
           "([^:/?#]*)"       ; | #1 = scheme-opt
           ":)?"              ; \
           "(?://"            ; / slash-slash-authority-opt
           "(?:"              ; | / user-at-opt
           "([^/?#@]*)"       ; | | #2 = user-opt
           "@)?"              ; | \
           "([^/?#:]*)?"      ; | #3 = host-opt
           "(?::"             ; | / colon-port-opt
           "([0-9]*)"         ; | | #4 = port-opt
           ")?"               ; | \
           ")?"               ; \
           "([^?#]*)"         ; #5 = path
           "(?:\\?"           ; / question-query-opt
           "([^#]*)"          ; | #6 = query-opt
           ")?"               ; \
           "(?:#"             ; / hash-fragment-opt
           "(.*)"             ; | #7 = fragment-opt
           ")?"               ; \
           "$")))

;; string->url : str -> url
;; Original version by Neil Van Dyke
(define (string->url str)
  (apply
   (lambda (scheme user host port path query fragment)
     (when (and scheme (not (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9+.-]*$"
                                           scheme)))
       (url-error "Invalid URL string; bad scheme ~e: ~e" scheme str))
     ;; Windows => "file://xxx:/...." specifies a "xxx:/..." path
     (let ([win-file? (and (or (equal? "" port) (not port))
                           (equal? "file" scheme)
                           (eq? 'windows (file-url-path-convention-type))
                           (not (equal? host "")))])
       (when win-file?
         (set! path (cond [(equal? "" port) (string-append host ":" path)]
                          [(and path host) (string-append host "/" path)]
                          [else (or path host)]))
         (set! port #f)
         (set! host ""))
       (let* ([scheme   (and scheme (string-downcase scheme))]
              [host     (and host (string-downcase host))]
              [user     (uri-decode/maybe user)]
              [port     (and port (string->number port))]
              [abs?     (or (equal? "file" scheme)
                            (regexp-match? #rx"^/" path))]
              [path     (if win-file?
                          (separate-windows-path-strings path)
                          (separate-path-strings path))]
              [query    (if query (form-urlencoded->alist query) '())]
              [fragment (uri-decode/maybe fragment)])
         (make-url scheme user host port abs? path query fragment))))
   (cdr (regexp-match url-regexp str))))

(define (uri-decode/maybe f) (friendly-decode/maybe f uri-decode))

(define (friendly-decode/maybe f uri-decode)
  ;; If #f, and leave unmolested any % that is followed by hex digit
  ;; if a % is not followed by a hex digit, replace it with %25
  ;; in an attempt to be "friendly"
  (and f (uri-decode (regexp-replace* #rx"%([^0-9a-fA-F])" f "%25\\1"))))

;; separate-path-strings : string[starting with /] -> (listof path/param)
(define (separate-path-strings str)
  (let ([strs (regexp-split #rx"/" str)])
    (map separate-params (if (string=? "" (car strs)) (cdr strs) strs))))

(define (separate-windows-path-strings str)
  (url-path (path->url (bytes->path (string->bytes/utf-8 str) 'windows))))

(define (separate-params s)
  (let ([lst (map path-segment-decode (regexp-split #rx";" s))])
    (make-path/param (car lst) (cdr lst))))

(define (path-segment-decode p)
  (cond [(string=? p "..") 'up]
        [(string=? p ".") 'same]
        [else (uri-path-segment-decode p)]))

(define (path-segment-encode p)
  (cond [(eq?    p 'up)   ".."]
        [(eq?    p 'same) "."]
        [(equal? p "..")  "%2e%2e"]
        [(equal? p ".")   "%2e"]
        [else (uri-path-segment-encode* p)]))

(define (combine-path-strings absolute? path/params)
  (cond [(null? path/params) ""]
        [else (let ([p (string-join (map join-params path/params) "/")])
                (if absolute? (string-append "/" p) p))]))

(define (join-params s)
  (string-join (map path-segment-encode
                    (cons (path/param-path s) (path/param-param s)))
               ";"))

(define (path->url path)
  (let* ([spath (simplify-path path #f)]
         [dir? (let-values ([(b n dir?) (split-path spath)]) dir?)]
         ;; If original path is a directory the resulting URL
         ;; should have a trailing forward slash
         [url-tail (if dir? (list (make-path/param "" null)) null)]
         [url-path
          (let loop ([path spath][accum null])
            (let-values ([(base name dir?) (split-path path)])
              (cond
                [(not base)
                 (append (map
                          (lambda (s)
                            (make-path/param s null))
                          (if (eq? (path-convention-type path) 'windows)
                              ;; For Windows, massage the root:
                              (let ([s (regexp-replace
                                        #rx"[/\\\\]$"
                                        (bytes->string/utf-8 (path->bytes name))
                                        "")])
                                (cond
                                  [(regexp-match? #rx"^\\\\\\\\[?]\\\\[a-zA-Z]:" s)
                                   ;; \\?\<drive>: path:
                                   (regexp-split #rx"[/\\]+" (substring s 4))]
                                  [(regexp-match? #rx"^\\\\\\\\[?]\\\\UNC" s)
                                   ;; \\?\ UNC path:
                                   (regexp-split #rx"[/\\]+" (substring s 7))]
                                  [(regexp-match? #rx"^[/\\]" s)
                                   ;; UNC path:
                                   (regexp-split #rx"[/\\]+" s)]
                                  [else
                                   (list s)]))
                              ;; On other platforms, we drop the root:
                              null))
                         accum)]
                [else
                 (let ([accum (cons (make-path/param
                                     (if (symbol? name)
                                         name
                                         (bytes->string/utf-8
                                          (path-element->bytes name)))
                                     null)
                                    accum)])
                   (if (eq? base 'relative)
                       accum
                       (loop base accum)))])))])
    (make-url "file" #f "" #f (absolute-path? path) (append url-path url-tail) '() #f)))


(define (url->path url [kind (system-path-convention-type)])
  (file://->path url kind))

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
  (let*-values
      ([(method) (case method
                   [(get) "GET"] [(post) "POST"] [(head) "HEAD"]
                   [(put) "PUT"] [(delete) "DELETE"]
                   [else (url-error "unsupported method: ~a" method)])]
       [(proxy) (assoc (url-scheme url) (current-proxy-servers))]
       [(server->client client->server) (make-ports url proxy)]
       [(access-string) (url->string
                         (if proxy
                           url
                           (make-url #f #f #f #f
                                     (url-path-absolute? url)
                                     (url-path url)
                                     (url-query url)
                                     (url-fragment url))))])
    (define (println . xs)
      (for-each (lambda (x) (display x client->server)) xs)
      (display "\r\n" client->server))
    (println method " " access-string " HTTP/1.0")
    (println "Host: " (url-host url)
             (let ([p (url-port url)]) (if p (format ":~a" p) "")))
    (when data (println "Content-Length: " (bytes-length data)))
    (for-each println strings)
    (println)
    (when data (display data client->server))
    (flush-output client->server)
    (tcp-abandon-port client->server)
    server->client))

(define current-url-encode-mode (make-parameter 'recommended))

(define (uri-encode* str)
  (case (current-url-encode-mode)
    [(unreserved) (uri-unreserved-encode str)]
    [(recommended) (uri-encode str)]))

(define (uri-path-segment-encode* str)
  (case (current-url-encode-mode)
    [(unreserved) (uri-path-segment-unreserved-encode str)]
    [(recommended) (uri-path-segment-encode str)]))

(provide (struct-out url) (struct-out path/param))

(provide/contract
 (string->url (-> (and/c string?
                         (or/c #rx"^[a-zA-Z][a-zA-Z0-9+.-]*:"
                               (not/c #rx"^[^:/?#]*:")))
                  url?))
 (path->url ((or/c path-string? path-for-some-system?) . -> . url?))
 (url->string (url? . -> . string?))
 (url->path (->* (url?) ((one-of/c 'unix 'windows)) path-for-some-system?))

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
 (display-pure-port (input-port? . -> . void?))
 (purify-port (input-port? . -> . string?))
 (get-pure-port/headers (->* (url?) ((listof string?) #:redirections exact-nonnegative-integer? #:status? boolean?) 
                             (values input-port? string?)))
 (netscape/string->url (string? . -> . url?))
 (call/input-url (case-> (-> url?
                             (-> url? input-port?)
                             (-> input-port? any)
                             any)
                         (-> url?
                             (-> url? (listof string?) input-port?)
                             (-> input-port? any)
                             (listof string?)
                             any)))
 (combine-url/relative (url? string? . -> . url?))
 (rename -url-exception? url-exception? (any/c . -> . boolean?))
 (current-proxy-servers
  (parameter/c (or/c false/c (listof (list/c string? string? number?)))))
 (file-url-path-convention-type
  (parameter/c (one-of/c 'unix 'windows)))
 (current-url-encode-mode (parameter/c (one-of/c 'recommended 'unreserved))))
