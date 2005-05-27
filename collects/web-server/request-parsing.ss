(module request-parsing mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           "util.ss"
           "connection-manager.ss"
           (lib "port.ss")
           )

  ;; the request struct as currently doc'd
  (define-struct request (method uri headers bindings host-ip client-ip))

  ;; path-prefix: (listof string)
  ;; The part of the URL path that maps to the servlet
  ;; path-suffix: (listof string)
  ;; The part of the URL path that gets passed to the servlet as arguments.

  ;; header?: anyd/c -> boolean
  ;; is this a header?
  (define header?
    (cons/c symbol? bytes?))

  ;; bindings? any/c -> boolean
  ;; is this a binding
  (define binding?
    (cons/c symbol? string?))

  (provide/contract
   [struct request ([method symbol?] [uri url?] [headers (listof header?)]
                    [bindings (union (listof binding?) string?)]
                    [host-ip string?]
                    [client-ip string?])]
   [read-request ((input-port?) . ->* . (request? boolean?))]
   [read-bindings (connection? symbol? url? (listof header?)
                               . -> . (union (listof binding?) string?))])

  ;; **************************************************
  ;; read-request: input-port -> request boolean?
  ;; read the request line, and the headers, determine if the connection should
  ;; be closed after servicing the request and build a request structure
  (define (read-request ip)
    (let-values ([(method uri major-version minor-version)
                  (read-request-line ip)])
      (let ([headers (read-headers ip)])
        (let-values ([(host-ip client-ip)
                      (if (tcp-port? ip)
                          (tcp-addresses ip)
                          (values "127.0.0.1" "127.0.0.1"))])
          (values
           (make-request method uri headers '() host-ip client-ip)
           (close-connection?
            headers major-version minor-version client-ip host-ip))))))

  ;; **************************************************
  ;; close-connection?

  ; close-connection? : (listof (cons symbol bytes)) number number string string -> boolean
  ; determine if this connection should be closed after serving the response
  (define (close-connection? headers major minor client-ip host-ip)
    (or (< major 1)
        (and (= major 1) (= minor 0))
        (cond
          [(assq 'connection headers)
           => (lambda (x) (string-ci=? "close" (bytes->string/utf-8 (cdr x))))]
          [else #f])
        (msie-from-local-machine? headers client-ip host-ip)))

  ; : table str str -> bool
  ; to work around a bug in MSIE for documents < 265 bytes when connecting from the local
  ; machine.  The server could pad the response as MSIIS does, but closing the connection works, too.
  ; We do not check for version numbers since IE 6 under windows is 5.2 under macosX
  (define (msie-from-local-machine? headers client-ip host-ip)
    (and (string=? host-ip client-ip)
         (cond
           [(or (assq 'HTTP_USER_AGENT headers)
                (assq 'user-agent headers))
            => (lambda (client)
                 (and (regexp-match MSIE-regexp (cdr client))
                      #t))]
           [else #f])))

  (define MSIE-regexp (regexp "MSIE"))

  ;; **************************************************
  ;; read-request-line

  ; Method = (U 'get 'post 'head 'put 'delete 'trace)
  (define METHOD:REGEXP
    (byte-regexp #"^(GET|HEAD|POST|PUT|DELETE|TRACE) (.+) HTTP/([0-9]+)\\.([0-9]+)$"))

  (define (match-method x)
    (regexp-match METHOD:REGEXP x))
  ;:(define match-method (type: (str -> (union false (list str str str str str)))))


  ; read-request-line : iport -> symbol url number number
  ; to read in the first line of an http request, AKA the "request line"
  ; effect: in case of errors, complain [MF: where] and close the ports
  (define (read-request-line ip)
    (let ([line (read-bytes-line ip 'any)])
      (if (eof-object? line)
          (error 'read-request "http input closed abruptly")
          (cond
            [(match-method line)
             => (lambda (x)
                  (values
                   (lowercase-symbol! (list-ref x 1))
                   (string->url (bytes->string/utf-8 (list-ref x 2)))
                   (string->number (bytes->string/utf-8 (list-ref x 3)))
                   (string->number (bytes->string/utf-8 (list-ref x 4)))))]
            [else (error 'read-request "malformed request ~a" line)]))))



  ;; **************************************************
  ;; read-headers

  ;(define COLON:REGEXP (regexp (format "^([^:]*):[ ~a]*(.*)" #\tab)))
  (define COLON:REGEXP (byte-regexp (bytes-append #"^([^:]*):[ " (bytes 9) #"]*(.*)")))

  (define (match-colon s)
    (regexp-match COLON:REGEXP s))
  ;:(define match-colon (type: (str -> (union false (list str str str)))))


  ; read-headers : iport -> (listof (cons symbol bytes))
  (define (read-headers in)
    (let read-header ()
      (let ([l (read-bytes-line in 'any)])
        (cond
          [(eof-object? l) null]
          [(zero? (bytes-length l)) null]
          [(match-colon l) =>
                           (lambda (match)
                             ; (cadr match) exists because COLON:REGEXP contains two (.)
                             ; (caddr match) exists because COLON:REGEXP contains two (.)
                             (cons (cons (lowercase-symbol! (cadr match))
                                         (read-one-head in (caddr match)))
                                   (read-header)))]
          [else (error 'read-headers "malformed header")]))))


  ; read-one-head : iport bytes -> bytes
  (define (read-one-head in rhs)
    (let ([c (peek-byte in)])
      (cond
        [(or (= c 32) (= c 9)) ;(or (eq? c #\space) (eq? c #\tab))

         ; (read-bytes-line in 'any) can't return eof
         ; because we just checked with peek-char
         ; Spidey: FLOW
         (read-one-head in (bytes-append rhs (read-bytes-line in 'any)))]
        [else rhs])))

  ;; **************************************************
  ;; read-bindings

  (define INPUT-BUFFER-SIZE 4096)

  ;; read-bindings: connection symboll url (listof header?) -> (union (listof binding?) string?)
  (define (read-bindings conn meth uri headers)
    (case meth
      [(get) (url-query uri)]
      [(post)
       (let ([content-type (assq 'content-type headers)])
         (cond
           [(and content-type (regexp-match FILE-FORM-REGEXP (cdr content-type)))
             => (lambda (content-boundary)
                  (map (lambda (part)
                         ;; more here - better checks, avoid string-append
                         (cons (get-field-name (cdr (assq 'content-disposition (car part))))
                               (apply string-append (cdr part))))
                       (read-mime-multipart (bytes->string/utf-8 (cadr content-boundary)) (connection-i-port conn))))]
             [else
             (let ([len-str (assq 'content-length headers)]
                   [in (connection-i-port conn)])
               (if len-str
                 (cond
                   [(string->number (bytes->string/utf-8 (cdr len-str)))
                    => (lambda (len) (read-string len in))]
                    [else (error "Post request contained a non-numeric content-length")])
                 (apply string-append
                        (let read-to-eof ()
                          (let ([s (read-string INPUT-BUFFER-SIZE in)])
                            (if (eof-object? s)
                              null
                              (cons s (read-to-eof))))))))]))]
      [else (error "unsupported method" meth)]))

  (define FILE-FORM-REGEXP (regexp "multipart/form-data; *boundary=(.*)"))

  ;; GregP: this is where I would get the filename out.
  ; get-field-name : str -> symbol
  (define (get-field-name rhs)
    (let ([x (regexp-match "name=(\"([^\"]*)\"|([^ ;]*))" rhs)])
      (unless x
        (error 'get-field-name "Couldn't extract form field name for file upload from ~a" x))
      (lowercase-symbol! (or (caddr x) (cadddr x)))))

  ;; **************************************************
  ;; read-mime-multipart

  ; read-mime-multipart : str iport -> (listof part)
  (define (read-mime-multipart boundary in)
    (let* ([boundary-len (string-length boundary)]
           [start-boundary (string-append "--" boundary)]
           [end-boundary (string-append start-boundary "--")])
      (let skip-preamble ()
        (let ([line (read-line in 'return-linefeed)])
          (cond
            [(string=? line start-boundary)
             (let read-parts ()
               (let ([headers (read-headers in)])
                 (let read-mime-part-body ([more-k (lambda (contents)
                                                     (cons (construct-mime-part
                                                            headers contents)
                                                           (read-parts)))]
                                           [end-k (lambda (contents)
                                                    (list (construct-mime-part
                                                           headers contents)))])
                   (let ([line (read-line in 'return-linefeed)])
                     (cond
                       [(string=? line start-boundary)
                        (more-k null)]
                       [(string=? line end-boundary)
                        (end-k null)]
                       [else (read-mime-part-body
                              (lambda (x) (more-k (cons line x)))
                              (lambda (x) (end-k (cons line x))))])))))]
            [(string=? line end-boundary) null]
            [else (skip-preamble)])))))

  ; more here - use structure, perhaps
  ; construct-mime-part : (listof header) (listof str) -> part
  (define (construct-mime-part headers body)
    (cons headers
          (cond
            [(null? body) null]
            [else (cons (car body)
                        (foldr (lambda (str acc)
                                 (list* CR-NL str acc))
                               null
                               (cdr body)))])))

  (define CR-NL (format "~a~a" #\return #\newline))


  )
