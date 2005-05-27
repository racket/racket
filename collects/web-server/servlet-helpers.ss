(module servlet-helpers mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           "util.ss"
           "response.ss"
           "request-parsing.ss"
           (lib "xml.ss" "xml")
           (lib "base64.ss" "net"))

  (provide extract-binding/single
           extract-bindings
           exists-binding?
           extract-user-pass
           build-suspender
           make-html-response/incremental
           report-errors-to-browser
           redirect-to
           permanently
           temporarily
           see-other
           (all-from-except "request-parsing.ss" request-bindings)
           (rename request-bindings request-bindings/raw)
           (rename get-parsed-bindings request-bindings)
           translate-escapes
           )

  ;; get-parsed-bindings : request -> (listof (cons sym str))
  (define (get-parsed-bindings r)
    (let ([x (request-bindings r)])
      (if (list? x)
          x
          (parse-bindings x))))

  ;; parse-bindings : (U #f String) -> (listof (cons Symbol String))
  (define (parse-bindings raw)
    (if (string? raw)
        (let ([len (string-length raw)])
          (let loop ([start 0])
            (let find= ([key-end start])
              (if (>= key-end len)
                  null
                  (if (eq? (string-ref raw key-end) #\=)
                      (let find-amp ([amp-end (add1 key-end)])
                        (if (or (= amp-end len) (eq? (string-ref raw amp-end) #\&))
                            (cons (cons (string->symbol (substring raw start key-end))
                                        (translate-escapes
                                         (substring raw (add1 key-end) amp-end)))
                                  (loop (add1 amp-end)))
                            (find-amp (add1 amp-end))))
                      (find= (add1 key-end)))))))
        null))

  ; extract-binding/single : sym (listof (cons str str)) -> str
  (define (extract-binding/single name bindings)
    (let ([lst (extract-bindings name bindings)])
      (cond
        [(null? lst)
         (error 'extract-binding/single "~a not found in ~a" name bindings)]
        [(null? (cdr lst)) (car lst)]
        [else (error 'extract-binding/single "~a occurs multiple times in ~a" name bindings)])))

  ; extract-bindings : sym (listof (cons str str)) -> (listof str)
  (define (extract-bindings name bindings)
    (map cdr (filter (lambda (x) (equal? name (car x))) bindings)))

  ; exists-binding? : sym (listof (cons sym str)) -> bool
  ; for checkboxes
  (define (exists-binding? name bindings)
    (if (assq name bindings)
        #t
        #f))

  ; build-suspender : (listof html) (listof html) [(listof (cons sym str))] [(listof (cons sym str))] -> str -> response
  (define build-suspender
    (opt-lambda (title content [body-attributes '([bgcolor "white"])] [head-attributes null])
      (lambda (k-url)
        `(html (head ,head-attributes
                     (meta ([http-equiv "Pragma"] [content "no-cache"])) ; don't cache in netscape
                     (meta ([http-equiv "expires"] [content "-1"])) ; don't cache in IE
                     ; one site said to use -1, another said to use 0.
                     (title . ,title))
               (body ,body-attributes
                     (form ([action ,k-url] [method "post"])
                           . ,content))))))

  ; redirection-status = (make-redirection-status nat str)
  (define-struct redirection-status (code message))

  (define permanently (make-redirection-status 301 "Moved Permanently"))
  (define temporarily (make-redirection-status 302 "Moved Temporarily"))
  (define see-other (make-redirection-status 303 "See Other"))

  ; : str [redirection-status] -> response
  (define redirect-to
    (opt-lambda (uri [perm/temp permanently])
      (make-response/full (redirection-status-code perm/temp)
                          (redirection-status-message perm/temp)
                          (current-seconds) #"text/html"
                          `((location . ,uri)) (list (redirect-page uri)))))

  ; : str -> str
  (define (redirect-page url)
    (xexpr->string `(html (head (meta ((http-equiv "refresh") (url ,url)))
                                "Redirect to " ,url)
                          (body (p "Redirecting to " (a ([href ,url]) ,url))))))

  ; make-html-response/incremental : ((string -> void) -> void) -> response/incremental
  (define (make-html-response/incremental chunk-maker)
    (make-response/incremental
     200 "Okay" (current-seconds) #"text/html" '()
     chunk-maker))

  ; : (response -> doesn't) -> void
  ; to report exceptions that occur later to the browser
  ; this must be called at the begining of a servlet
  (define (report-errors-to-browser send/finish-or-back)
    (current-exception-handler
     (lambda (exn)
       (send/finish-or-back
        `(html (head (title "Servlet Error"))
               (body ([bgcolor "white"])
                     (p "The following error occured: "
                        (pre ,(exn->string exn)))))))))

  ; Authentication

  (define AUTHENTICATION-REGEXP (regexp "([^:]*):(.*)"))
  (define (match-authentication x) (regexp-match AUTHENTICATION-REGEXP x))
  ;:(define match-authentication (type: (str -> (union false (list str str str)))))

  ; extract-user-pass : (listof (cons sym bytes)) -> (U #f (cons str str))
  ;; Notes (GregP)
  ;; 1. This is Basic Authentication (RFC 1945 SECTION 11.1)
  ;;    e.g. an authorization header will look like this:
  ;;         Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
  ;; 2. Headers should be read as bytes and then translated to unicode as appropriate.
  ;; 3. The Authorization header should have bytes (i.e. (cdr pass-pair) is bytes
  (define (extract-user-pass headers)
    (let ([pass-pair (assq 'authorization headers)])
      (and pass-pair
           (let ([basic-credentials (cdr pass-pair)])
             (cond
              [(and (basic? basic-credentials)
                    (match-authentication
                     (base64-decode (subbytes basic-credentials 6 (bytes-length basic-credentials))))
                     )
               => (lambda (user-pass)
                    (cons (cadr user-pass) (caddr user-pass)))]
              [else #f])))))

  ;; basic?: bytes -> (union (listof bytes) #f)
  ;; does the second part of the authorization header start with #"Basic "
  (define basic?
    (let ([basic-regexp (byte-regexp #"^Basic .*")])
      (lambda (some-bytes)
        (regexp-match basic-regexp some-bytes))))

  )
