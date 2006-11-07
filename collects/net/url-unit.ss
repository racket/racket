;; To do:
;;   Handle HTTP/file errors.
;;   Not throw away MIME headers.
;;     Determine file type.

;; ----------------------------------------------------------------------

;; Input ports have two statuses:
;;   "impure" = they have text waiting
;;   "pure" = the MIME headers have been read

(module url-unit mzscheme
  (require (lib "file.ss")
           (lib "unitsig.ss")
           (lib "port.ss")
           (lib "string.ss")
           (lib "list.ss")
           "url-structs.ss"
           "uri-codec.ss"
           "url-sig.ss"
           "tcp-sig.ss")
  (provide url@)

  ;; undocumented hook to allow testing
  (provide set-url:os-type!)
  (define url:os-type (system-type))
  (define (set-url:os-type! new) (set! url:os-type new))

  (define url@
    (unit/sig net:url^
      (import net:tcp^)

      (define-struct (url-exception exn:fail) ())

      (define current-proxy-servers
        (make-parameter null
          (lambda (v)
            (unless (and (list? v)
                         (andmap
                          (lambda (v)
                            (and (list? v)
                                 (= 3 (length v))
                                 (equal? (car v) "http")
                                 (string? (car v))
                                 (number? (caddr v))
                                 (exact? (caddr v))
                                 (integer? (caddr v))
                                 (<= 1 (caddr v) 65535)))
                          v))
              (raise-type-error
               'current-proxy-servers
               "list of list of scheme, string, and exact integer in [1,65535]"
               v))
            (apply
             list-immutable
             (map (lambda (v)
                    (list-immutable (string->immutable-string (car v))
                                    (string->immutable-string (cadr v))
                                    (caddr v)))
                  v)))))

      (define (url-error fmt . args)
        (let ([s (string->immutable-string
                  (apply format fmt
                         (map (lambda (arg)
                                (if (url? arg) (url->string arg) arg))
                              args)))])
          (raise (make-url-exception s (current-continuation-marks)))))

      (define (url->string url)
        (let ([scheme (url-scheme url)]
              [user   (url-user url)]
              [host   (url-host url)]
              [port   (url-port url)]
              [path   (url-path url)]
              [query  (url-query url)]
              [fragment (url-fragment url)]
              [sa string-append])
          (sa (if scheme (sa scheme ":") "")
              (if (or user host port)
                (sa "//"
                    (if user (sa (uri-encode user) "@") "")
                    (if host host "")
                    (if port (sa ":" (number->string port)) "")
                    ;; There used to be a "/" here, but that causes an
                    ;; extra leading slash -- wonder why it ever worked!
                    )
                "")
              (combine-path-strings (url-path-absolute? url) path)
              ;; (if query (sa "?" (uri-encode query)) "")
              (if (null? query) "" (sa "?"  (alist->form-urlencoded query)))
              (if fragment (sa "#" (uri-encode fragment)) ""))))

      ;; url->default-port : url -> num
      (define (url->default-port url)
        (let ([scheme (url-scheme url)])
          (cond [(not scheme) 80]
                [(string=? scheme "http") 80]
                [(string=? scheme "https") 443]
                [else (url-error "Scheme ~a not supported" (url-scheme url))])))

      ;; make-ports : url -> in-port x out-port
      (define (make-ports url proxy)
        (let ([port-number (if proxy
                             (caddr proxy)
                             (or (url-port url) (url->default-port url)))]
              [host (if proxy
                      (cadr proxy)
                      (url-host url))])
          (tcp-connect host port-number)))

      ;; http://getpost-impure-port : bool x url x union (str, #f) x list (str) -> in-port
      (define (http://getpost-impure-port get? url post-data strings)
        (let*-values
            ([(proxy) (assoc (url-scheme url) (current-proxy-servers))]
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
          (println (if get? "GET " "POST ") access-string " HTTP/1.0")
          (println "Host: " (url-host url)
                   (let ([p (url-port url)]) (if p (format ":~a" p) "")))
          (when post-data (println "Content-Length: " (bytes-length post-data)))
          (for-each println strings)
          (println)
          (when post-data (display post-data client->server))
          (flush-output client->server)
          (tcp-abandon-port client->server)
          server->client))

      (define (file://->path url)
        ;; remove all ""s
        (let ([elts (remove* '("") (map path/param-path (url-path url)))]
              [abs? (url-path-absolute? url)])
          ;; See the discussion in PR8060 for an explanation
          (if (eq? 'windows url:os-type)
            (let ([host (or (url-host url) "")])
              (unless (equal? "" host) (set! elts (cons host elts)))
              (if (null? elts)
                (build-path) ; make it throw the error
                (let* ([fst (car elts)] [len (string-length fst)])
                  (if (or (not abs?) (eq? #\: (string-ref fst (sub1 len))))
                    (apply build-path elts)
                    (if (null? (cdr elts))
                      (build-path (string-append "\\\\" (car elts)))
                      (apply build-path
                             (string-append "\\\\" (car elts) "\\" (cadr elts))
                             (cddr elts)))))))
            (apply build-path (if abs? (cons "/" elts) elts)))))

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
                [(or (string=? scheme "http")
                     (string=? scheme "https"))
                 (http://getpost-impure-port get? url post-data strings)]
                [(string=? scheme "file")
                 (url-error "There are no impure file: ports")]
                [else (url-error "Scheme ~a unsupported" scheme)])))

      ;; get-impure-port : url [x list (str)] -> in-port
      (define get-impure-port
        (case-lambda
          [(url) (get-impure-port url '())]
          [(url strings) (getpost-impure-port #t url #f strings)]))

      ;; post-impure-port : url x bytes [x list (str)] -> in-port
      (define post-impure-port
        (case-lambda
          [(url post-data) (post-impure-port url post-data '())]
          [(url post-data strings)
           (getpost-impure-port #f url post-data strings)]))

      ;; getpost-pure-port : bool x url x list (str) -> in-port
      (define (getpost-pure-port get? url post-data strings)
        (let ([scheme (url-scheme url)])
          (cond [(not scheme)
                 (schemeless-url url)]
                [(or (string=? scheme "http")
                     (string=? scheme "https"))
                 (let ([port (http://getpost-impure-port
                              get? url post-data strings)])
                   (with-handlers ([void (lambda (exn)
                                           (close-input-port port)
                                           (raise exn))])
                     (purify-port port))
                   port)]
                [(string=? scheme "file")
                 (file://get-pure-port url)]
                [else (url-error "Scheme ~a unsupported" scheme)])))

      ;; get-pure-port : url [x list (str)] -> in-port
      (define get-pure-port
        (case-lambda
          [(url) (get-pure-port url '())]
          [(url strings) (getpost-pure-port #t url #f strings)]))

      ;; post-pure-port : url bytes [x list (str)] -> in-port
      (define post-pure-port
        (case-lambda
          [(url post-data) (post-pure-port url post-data '())]
          [(url post-data strings) (getpost-pure-port #f url post-data strings)]))

      ;; display-pure-port : in-port -> ()
      (define (display-pure-port server->client)
        (copy-port server->client (current-output-port))
        (close-input-port server->client))

      (define (empty-url? url)
        (and (not (url-scheme url))
             (not (url-query url))
             (not (url-fragment url))
             (null? (url-path url))))

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
      ;; the strange cases 2 and 4 implicitly change urls
      ;; with paths segments "." and ".." at the end
      ;; into "./" and "../" respectively
      (define (remove-dot-segments path)
        (let loop ([path path]
                   [result '()])
          (cond
            [(null? path) (reverse result)]
            [(and (eq? (path/param-path (car path)) 'same)
                  (null? (cdr path)))
             (loop (cdr path)
                   (cons (make-path/param "" '()) result))]
            [(eq? (path/param-path (car path)) 'same)
             (loop (cdr path)
                   result)]
            [(and (eq? (path/param-path (car path)) 'up)
                  (null? (cdr path))
                  (not (null? result)))
             (loop (cdr path) 
                   (cons (make-path/param "" '()) (cdr result)))]
            [(and (eq? (path/param-path (car path)) 'up)
                  (not (null? result)))
             (loop (cdr path) (cdr result))]
            [(and (eq? (path/param-path (car path)) 'up)
                  (null? result))
             ;; when we go up too far, just drop the "up"s.
             (loop (cdr path) result)]
            [else
             (loop (cdr path) (cons (car path) result))])))

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
                  #rx"^HTTP/.*?((\r\n\r\n)|(\n\n)|(\r\r))" port)])
          (if m
            (read-string (cdar m) port)
            "")))

      (define character-set-size 256)

      ;; netscape/string->url : str -> url
      (define (netscape/string->url string)
        (let ([url (string->url string)])
          (if (url-scheme url)
            url
            (if (string=? string "")
              (url-error "Can't resolve empty string as URL")
              (begin
                (set-url-scheme! url
                  (if (char=? (string-ref string 0) #\/) "file" "http"))
                url)))))

      ;; string->url : str -> url
      ;; New implementation, mostly provided by Neil Van Dyke
      (define url-rx
        (regexp (string-append
                 "^"
                 "[ \t\f\r\n]*"
                 "(?:"                ; <A  front-opt
                 "(?:([a-zA-Z]*):)?"  ; =1  scheme-colon-opt
                 "(?:"                ; <B  slashslash-opt
                 "//"
                 "(?:([^:/@;?#]*)@)?" ; =2  user-at-opt
                 "([^:/@;?#]*)?"      ; =3  host-opt
                 "(?::([0-9]*))?"     ; =4  colon-port-opt
                 ")?"                 ; >B  slashslash-opt
                 ")?"                 ; >A  front-opt
                 "([^?#]*)"           ; =5  path
                 "(?:\\?([^#]*))?"    ; =6  question-query-opt
                 "(?:#(.*))?"         ; =7  hash-fragment-opt
                 "[ \t\f\r\n]*"
                 "$")))
      (define (string->url str)
        (apply
         (lambda (scheme user host port path query fragment)
           ;; Windows => "file://xxx:/...." specifies a "xxx:/..." path
           (when (and (equal? "" port) (equal? "file" scheme)
                      (eq? 'windows url:os-type))
             (set! path (string-append host ":" path))
             (set! host #f))
           (let* ([user   (uri-decode/maybe user)]
                  [port   (and port (string->number port))]
                  [abs?   (and (not (= 0 (string-length path)))
                               (char=? #\/ (string-ref path 0)))]
                  [path   (separate-path-strings
                           ;; If path is "" and the input is an absolute URL
                           ;; with a hostname, then the intended path is "/",
                           ;; but the URL is missing a "/" at the end.
                           path
                           #;
                           (if (and (string=? path "") host) "/" path))]
                  [query   (if query (form-urlencoded->alist query) '())]
                  [fragment (uri-decode/maybe fragment)])
             (when (string? scheme) (string-lowercase! scheme))
             (when (string? host) (string-lowercase! host))
             (make-url scheme user host port abs? path query fragment)))
         (cdr (or (regexp-match url-rx str)
                  (url-error "Invalid URL string: ~e" str)))))

      (define (uri-decode/maybe f)
        ;; If #f, and leave unmolested any % that is followed by hex digit
        ;; if a % is not followed by a hex digit, replace it with %25
        ;; in an attempt to be "friendly"
        (and f (uri-decode (regexp-replace* "%([^0-9a-fA-F])" f "%25\\1"))))

      ;; separate-path-strings : string[starting with /] -> (listof path/param)
      (define (separate-path-strings str)
        (let ([strs (regexp-split #rx"/" str)])
          (map separate-params (if (string=? "" (car strs)) (cdr strs) strs))))

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
              [else (uri-path-segment-encode p)]))

      (define (combine-path-strings absolute? path/params)
        (cond [(null? path/params) ""]
              [else (let ([p (join "/" (map join-params path/params))])
                      (if absolute? (string-append "/" p) p))]))

      (define (join-params s)
        (join ";" (map path-segment-encode
                       (cons (path/param-path s) (path/param-param s)))))

      (define (join sep strings)
        (cond [(null? strings) ""]
              [(null? (cdr strings)) (car strings)]
              [else
               (let loop ([strings (cdr strings)] [r (list (car strings))])
                 (if (null? strings)
                   (apply string-append (reverse! r))
                   (loop (cdr strings) (list* (car strings) sep r))))]))

      )))
