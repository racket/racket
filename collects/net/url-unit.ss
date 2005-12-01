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
	   "url-structs.ss"
           "uri-codec.ss"
           "url-sig.ss"
           "tcp-sig.ss")
  (provide url@)

  (define url@
    (unit/sig net:url^
      (import net:tcp^)

      (define-struct (url-exception exn:fail) ())

      (define current-proxy-servers
        (make-parameter null (lambda (v)
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

      (define url-error
        (lambda (fmt . args)
          (let ((s (string->immutable-string
                    (apply format fmt (map (lambda (arg)
                                             (if (url? arg)
                                                 (url->string arg)
                                                 arg))
                                           args)))))
            (raise (make-url-exception s (current-continuation-marks))))))

      (define (url->file-path url)
	(path->string
	 (apply build-path (or (url-host url) 'same)
		(map (lambda (x) (if (equal? x "") 'same x)) (url-path url)))))

      (define url->string
        (lambda (url)
          (let ((scheme (url-scheme url))
                (user (url-user url))
                (host (url-host url))
                (port (url-port url))
                (path (url-path url))
                (query (url-query url))
                (fragment (url-fragment url)))
            (cond
             ((and scheme (string=? scheme "file"))
              (string-append "file:" 
			     (url->file-path url)
                             (or (and (not fragment) "")
                                 (string-append "#" fragment))))
             (else
              (let ((sa string-append))
                (sa (if scheme (sa scheme "://") "")
                    (if user (sa (uri-encode user) "@") "")
                    (if host host "")
                    (if port (sa ":" (number->string port)) "")
                                        ; There used to be a "/" here, but that causes an
                                        ; extra leading slash -- wonder why it ever worked!
                    (combine-path-strings path)
                    ;(if query (sa "?" (uri-encode query)) "")
                    (if (null? query) "" (sa "?"  (alist->form-urlencoded query)))
                    (if fragment (sa "#" (uri-encode fragment)) ""))))))))

      ;; url->default-port : url -> num
      (define url->default-port
        (lambda (url)
          (let ((scheme (url-scheme url)))
            (cond
             ((not scheme) 80)
             ((string=? scheme "http") 80)
             (else
              (url-error "Scheme ~a not supported" (url-scheme url)))))))

      ;; make-ports : url -> in-port x out-port
      (define make-ports
        (lambda (url proxy)
          (let ((port-number (if proxy
                                 (caddr proxy)
                                 (or (url-port url)
                                     (url->default-port url))))
                (host (if proxy
                          (cadr proxy)
                          (url-host url))))
            (tcp-connect host port-number))))

      ;; http://getpost-impure-port : bool x url x union (str, #f) x list (str) -> in-port
      (define http://getpost-impure-port
        (lambda (get? url post-data strings)
          (let*-values (((proxy) (assoc (url-scheme url) (current-proxy-servers)))
                        ((server->client client->server)
                         (make-ports url proxy)))
            (let ((access-string
                   (url->string
                    (if proxy
                        url
                        (make-url #f #f #f #f
                                  (url-path url)
                                  (url-query url)
                                  (url-fragment url))))))
              (for-each (lambda (s)
                          (display (string-append s "\r\n") client->server))
                        (cons (format "~a ~a HTTP/1.0" (if get? "GET" "POST") access-string)
                              (cons (format "Host: ~a" (url-host url))
                                    (if post-data
                                        (cons
                                         (format "Content-Length: ~a" (bytes-length post-data))
                                         strings)
                                        strings)))))
            (display "\r\n" client->server)
            (when post-data
              (display post-data client->server))
	    (tcp-abandon-port client->server) ; flushes
            server->client)))

      ;; file://get-pure-port : url -> in-port
      (define file://get-pure-port
        (lambda (url)
          (open-input-file (url->file-path url))))

      (define (schemeless-url url)
        (url-error "Missing protocol (usually \"http:\") at the beginning of URL: ~a" url))

      ;; getpost-impure-port : bool x url x list (str) -> in-port
      (define getpost-impure-port
        (lambda (get? url post-data strings)
          (let ((scheme (url-scheme url)))
            (cond
             ((not scheme)
              (schemeless-url url))
             ((string=? scheme "http")
              (http://getpost-impure-port get? url post-data strings))
             ((string=? scheme "file")
              (url-error "There are no impure file: ports"))
             (else
              (url-error "Scheme ~a unsupported" scheme))))))

      ;; get-impure-port : url [x list (str)] -> in-port
      (define get-impure-port
        (case-lambda
         [(url) (get-impure-port url '())]
         [(url strings) (getpost-impure-port #t url #f strings)]))

      ;; post-impure-port : url x bytes [x list (str)] -> in-port
      (define post-impure-port
        (case-lambda
         [(url post-data) (post-impure-port url post-data '())]
         [(url post-data strings) (getpost-impure-port #f url post-data strings)]))

      ;; getpost-pure-port : bool x url x list (str) -> in-port
      (define getpost-pure-port
        (lambda (get? url post-data strings)
          (let ((scheme (url-scheme url)))
            (cond
             ((not scheme)
              (schemeless-url url))
             ((string=? scheme "http")
              (let ((port (http://getpost-impure-port get? url post-data strings)))
                (with-handlers ([void (lambda (exn)
                                        (close-input-port port)
                                        (raise exn))])
                  (purify-port port))
                port))
             ((string=? scheme "file")
              (file://get-pure-port url))
             (else
              (url-error "Scheme ~a unsupported" scheme))))))

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
      (define display-pure-port
        (lambda (server->client)
          (copy-port server->client (current-output-port))
          (close-input-port server->client)))

      (define empty-url?
        (lambda (url)
          (and (not (url-scheme url))
               (not (url-query url))
               (not (url-fragment url))
               (null? (url-path url)))))

      (define (combine-url/relative base string)
        (let ([relative (string->url string)])
          (cond
            [(empty-url? base)                ; Step 1
             relative]
            [(empty-url? relative)            ; Step 2a
             base]
            [(url-scheme relative)            ; Step 2b
             relative]
            [else                             ; Step 2c
             (set-url-scheme! relative (url-scheme base))
             (cond
               [(url-host relative)     ; Step 3
                relative]
               [else
                (set-url-host! relative (url-host base))
                (set-url-port! relative (url-port base)) ; Unspecified!
                (let ([rel-path (url-path relative)])
                  (cond
                    [(and (not (equal? string "")) ; Step 4
                          (char=? #\/ (string-ref string 0)))
                     relative]
                    [(or (not rel-path) ; Step 5
                         (null? rel-path))
                     (set-url-path! relative (url-path base))
                     (when (url-query relative)
                       (set-url-query! relative (url-query base)))
                     relative]
                    [else               ; Step 6
                     (merge-and-normalize
                      (url-path base) relative)]))])])))

      (define (merge-and-normalize base-path relative-url)
        (let* ([joined
                (let loop ([base-path base-path])
                  (cond
                    [(null? base-path) (url-path relative-url)]
                    [(null? (cdr base-path)) (url-path relative-url)]
                    [else (cons (car base-path) (loop (cdr base-path)))]))]
               [reversed/simplified
                (if (null? joined)
                    null
                    (let loop ([segs (reverse joined)])
                      (cond
                        [(null? segs) null]
                        [else (let ([fst (car segs)])
                                (cond
                                  [(string=? fst ".")
                                   (loop (cdr segs))]
                                  [(string=? fst "..")
                                   (if (null? (cdr segs))
                                       segs
                                       (loop (cddr segs)))]
                                  [else (cons (car segs) (loop (cdr segs)))]))])))])
          (set-url-path! relative-url (reverse reversed/simplified))
          relative-url))

      ;; call/input-url : url x (url -> in-port) x (in-port -> T)
      ;;                  [x list (str)] -> T
      (define call/input-url
        (let ((handle-port (lambda (server->client handler)
                             (dynamic-wind (lambda () 'do-nothing)
                                 (lambda () (handler server->client))
                                 (lambda () (close-input-port server->client))))))
          (case-lambda
           ((url getter handler)
            (handle-port (getter url) handler))
           ((url getter handler params)
            (handle-port (getter url params) handler)))))

      ;; purify-port : in-port -> header-string
      (define purify-port
        (lambda (port)
          (let ([m (regexp-match-peek-positions #rx"^HTTP/.*?((\r\n\r\n)|(\n\n)|(\r\r))" port)])
            (if m
                (read-string (cdar m) port)
                ""))))

      (define character-set-size 256)

      ;; netscape/string->url : str -> url
      (define netscape/string->url
        (lambda (string)
          (let ((url (string->url string)))
            (if (url-scheme url)
                url
                (if (string=? string "")
                    (url-error "Can't resolve empty string as URL")
                    (begin
                      (set-url-scheme! url
                                       (if (char=? (string-ref string 0) #\/)
                                           "file"
                                           "http"))
                      url))))))

      ;; string->url : str -> url
      ;; New implementation, mostly provided by Neil Van Dyke
      (define string->url
        (let ((rx (regexp (string-append
                           "^"
                           "[ \t\f\r\n]*"
                           "("                ; <1  front-opt
                           "([a-zA-Z]*:)?"    ; =2  scheme-colon-opt
                           "("                ; <3  slashslash-opt
                           "//"
                           "([^:/@;?#]*@)?"   ; =4  user-at-opt
                           "([^:/@;?#]+)?"    ; =5  host-opt
                           "(:[0-9]*)?"       ; =6  colon-port-opt
                           ")?"               ; >3  slashslash-opt
                           ")?"               ; >1  front-opt
                           "([^?#]*)"         ; =7  path
                           "(\\?[^#]*)?"      ; =8  question-query-opt
                           "(#.*)?"           ; =9 hash-fragment-opt
                           "[ \t\f\r\n]*"
                           "$"))))
          (lambda (str)
            (let ([m (regexp-match #rx"^[ \t\f\r\n]*file:(.*)$" str)])
              ;; File scheme:
              (if m
                  (let ([path+fragment (regexp-match #rx"^([^#]*)(#(.*))?$" (cadr m))])
                    (let ([path (cadr path+fragment)]
                          [fragment (caddr path+fragment)])
                      (if (or (relative-path? path)
                              (absolute-path? path))
			  (let-values ([(root elems kind)
					(let loop ([path (simplify-path path)][accum null][kind #f])
					  (let-values ([(base name dir?) (split-path path)])
					    (let ([kind (or kind
							    (if dir? 'dir 'file))])
					      (cond
					       [(path? base)
						(loop base (cons name accum) kind)]
					       [(eq? base 'relative)
						(values #f (cons name accum) kind)]
					       [else
						(values path accum kind)]))))])
			    (make-url "file"
				      #f  ; user
				      (and root (path->string root))  ; host
				      #f  ; port
				      (append (map path->string elems)
					      (if (eq? kind 'dir)
						  '("")
						  null))
				      '()  ; query
				      fragment))
                          (url-error "scheme 'file' path ~s neither relative nor absolute" path))))
                  ;; Other scheme:
                  (let ((match (regexp-match-positions rx str)))
                    (if match
                        (let* ((get-str (lambda (pos skip-left skip-right)
                                          (let ((pair (list-ref match pos)))
                                            (if pair
                                                (substring str
                                                           (+ (car pair) skip-left)
                                                           (- (cdr pair) skip-right))
                                                #f))))
                               (get-num (lambda (pos skip-left skip-right)
                                          (let ((s (get-str pos skip-left skip-right)))
                                            (if s (string->number s) #f))))
                               (host (get-str 5  0 0)))
                          (make-url (get-str 2  0 1) ; scheme
                                    (uri-decode/maybe (get-str 4  0 1)) ; user
                                    host
                                    (get-num 6  1 0) ; port
                                    (separate-path-strings
                                     (let ([path (get-str 7  0 0)])
                                       ;; If path is "" and the input is an absolute URL
                                       ;; with a hostname, then the intended path is "/",
                                       ;; but the URL is missing a "/" at the end.
                                       (if (and (string=? path "")
                                                host)
                                           "/"
                                           path)))
                                    ;(uri-decode/maybe (get-str 8  1 0)) ;
                                    ;query
                                    (let ([q (get-str 8 1 0)])
                                      (if q (form-urlencoded->alist q) '()))
                                    (uri-decode/maybe (get-str 9  1 0)) ; fragment
                                    ))
                        (url-error "Invalid URL string: ~e" str))))))))

      (define (uri-decode/maybe f)
        ;; If #f, and leave unmolested any % that is followed by hex digit
        ;; if a % is not followed by a hex digit, replace it with %25
        ;; in an attempt to be "friendly"
        (and f (uri-decode (regexp-replace* "%([^0-9a-fA-F])" f "%25\\1"))))

      ;; separate-path-strings : string[starting with /] -> (listof (union string path/param))
      (define (separate-path-strings str)
        (cond
          [(string=? str "") '()]
          [else
           (let loop ([str (if (char=? #\/ (string-ref str 0))
                               (substring str 1 (string-length str))
                               str)])
             (cond
               [(regexp-match #rx"([^/]*)/(.*)$" str)
                =>
                (lambda (m)
                  (cons (maybe-separate-params (cadr m)) (loop (caddr m))))]
               [else (list (maybe-separate-params str))]))]))

      (define (maybe-separate-params s)
        (cond
          [(regexp-match #rx"^([^;]*);(.*)$" s)
           =>
           (lambda (m)
             (make-path/param (cadr m) (caddr m)))]
          [else s]))

      (define (combine-path-strings strs)
        (apply
         string-append
         (let loop ([strs strs])
           (cond
             [(null? strs) '()]
             [else (list* "/"
                          (maybe-join-params (car strs))
                          (loop (cdr strs)))]))))

      ;; needs to unquote things!
      (define (maybe-join-params s)
        (cond
          [(string? s) s]
          [else (string-append (path/param-path s)
                               ";"
                               (path/param-param s))])))))
