(module servlet-helpers mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "xml.ss" "xml")
           (lib "base64.ss" "net")
           (lib "url.ss" "net")
           (lib "struct.ss"))
  (require "util.ss"
           "response.ss"
           "request-parsing.ss"
           "servlet-tables.ss")
  (provide get-host
           extract-binding/single
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
           (all-from "request-parsing.ss")
           (rename get-parsed-bindings request-bindings)
           translate-escapes)
  
  ;; URL parsing
  (provide (struct servlet-url (protocol host port servlets-root instance-id k-id nonce servlet-path extra-path))
           servlet-url->url-string
           servlet-url->url-string/no-continuation
           servlet-url->servlet-url/no-extra-path 
           request->servlet-url
           uri->servlet-url)
  (define-struct servlet-url (protocol host port servlets-root instance-id k-id nonce servlet-path extra-path))
  (define (servlet-url->url-string/no-continuation su)
    (url->string
     (make-url (servlet-url-protocol su)
               #f
               #f ;(servlet-url-host su)
               #f ;(servlet-url-port su)
               (append (servlet-url-servlets-root su)
                       (servlet-url-servlet-path su)
                       (servlet-url-extra-path su))
               empty
               #f)))
  (define (servlet-url->url-string su)
    (url->string
     (make-url (servlet-url-protocol su)
               #f
               #f ;(servlet-url-host su)
               #f ;(servlet-url-port su)
               (append (reverse (rest (reverse (servlet-url-servlets-root su))))
                       (list (make-path/param (first (reverse (servlet-url-servlets-root su)))
                                              (format "~a*~a*~a" 
                                                      (servlet-url-instance-id su)
                                                      (servlet-url-k-id su) 
                                                      (servlet-url-nonce su))))
                       (servlet-url-servlet-path su)
                       (servlet-url-extra-path su))
               empty
               #f)))
  (define (servlet-url->servlet-url/no-extra-path su)
    (copy-struct servlet-url su
                 [servlet-url-extra-path empty]))
  (define (request->servlet-url req)
    (uri->servlet-url (request-uri req)
                      (request-host-ip req)
                      (request-host-port req)))
  (define uri->servlet-url
    (opt-lambda (uri [default-host #f] [default-port #f])
      (let-values ([(k-instance k-id k-salt)
                    (let ([k-parts (continuation-url? uri)])
                      (if k-parts
                          (apply values k-parts)
                          (values #f #f #f)))])
        (let loop ([path (url-path uri)]
                   [servlets-root empty]
                   [found-servlets-root? #f]
                   [servlet-path empty]
                   [found-servlet-path? #f]
                   [extra-path empty])
          #;(printf "~S~n" (list path
                                 servlets-root found-servlets-root?
                                 servlet-path found-servlet-path?
                                 extra-path))
          (let ([top (if (empty? path)
                         #f
                         (first path))])
            (cond 
              ;; Find the servlets-root
              [(and top
                    (not found-servlets-root?)
                    ; XXX: Ack!
                    (not (or (and (not (empty? servlets-root))
                                  (string=? "servlets" (first (reverse servlets-root))))
                             (path/param? top))))
               (loop (rest path)
                     (append servlets-root (list top)) #f
                     servlet-path #f
                     extra-path)]
              ;;; if there is a continuation part
              [(and top
                    (not found-servlets-root?)
                    (path/param? top))
               (loop (rest path)
                     (append servlets-root (list (path/param-path top))) #t
                     servlet-path #f
                     extra-path)]
              ;;; if there is not
              [(and top
                    (not found-servlets-root?)
                    ; XXX: Ack!
                    (not (empty? servlets-root))
                    (string=? "servlets" (first (reverse servlets-root))))
               (loop path
                     servlets-root #t
                     servlet-path #f
                     extra-path)]
              ;; Find the servlet path
              [(and top
                    found-servlets-root?
                    (not found-servlet-path?)
                    (not (and (string? top)
                              (regexp-match ".ss$" top))))
               (loop (rest path)
                     servlets-root #t
                     (append servlet-path (list top)) #f
                     extra-path)]
              [(and top
                    found-servlets-root?
                    (not found-servlet-path?)
                    (and (string? top)
                         (regexp-match ".ss$" top)))
               (loop (rest path)
                     servlets-root #t
                     (append servlet-path (list top)) #t
                     extra-path)]
              ;; Compute the servlet-url
              [(and found-servlets-root?
                    found-servlet-path?)
               (make-servlet-url (url-scheme uri)
                                 (or (url-host uri) default-host)
                                 (or (url-port uri) default-port)
                                 servlets-root
                                 k-instance
                                 k-id
                                 k-salt
                                 servlet-path
                                 path)]
              [(empty? path)
               (error 'request->servlet-url "Not servlet-url: ~S; parsed: ~S" (url->string uri)
                      (list path
                            servlets-root found-servlets-root?
                            servlet-path found-servlet-path?
                            extra-path))]))))))
  
  ;; get-host : Url (listof (cons Symbol String)) -> Symbol
  ;; host names are case insesitive---Internet RFC 1034
  (define DEFAULT-HOST-NAME '<none>)
  (define (get-host uri headers)
    (cond
      [(url-host uri) => string->symbol]
      [(assq 'host headers)
       =>
       (lambda (h) (string->symbol (bytes->string/utf-8 (cdr h))))]
      [else DEFAULT-HOST-NAME]))
  
  ;; get-parsed-bindings : request -> (listof (cons sym str))
  (define (get-parsed-bindings r)
    (let ([x (request-bindings/raw r)])
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
                           ,@content))))))
  
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
        (regexp-match basic-regexp some-bytes)))))