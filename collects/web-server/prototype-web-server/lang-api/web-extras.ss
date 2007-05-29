(module web-extras mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "plt-match.ss")
           (lib "base64.ss" "net")
           (lib "url.ss" "net")
           "../../request-structs.ss"
           "../../response-structs.ss"
           "../private/web.ss")  
  (provide send/suspend/dispatch
           redirect/get)
  
  (define-syntax send/suspend/dispatch
    (syntax-rules ()
      [(_ response-generator)
       (extract-proc/url
        (send/suspend/url
         (lambda (k-url)
           (response-generator
            (lambda (proc)
              (embed-proc/url k-url proc))))))]))  
  
  (define (redirect/get)
    (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
    
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
                          `((Location . ,uri)) (list))))
  
  ; make-html-response/incremental : ((string -> void) -> void) -> response/incremental
  (define (make-html-response/incremental chunk-maker)
    (make-response/incremental
     200 "Okay" (current-seconds) #"text/html" '()
     chunk-maker))
  
  ; Authentication  
  ; basic-auth-extract-user-pass : (listof (cons sym bytes)) -> (or/c #f (cons str str))
  ;; Notes (GregP)
  ;; 1. This is Basic Authentication (RFC 1945 SECTION 11.1)
  ;;    e.g. an authorization header will look like this:
  ;;         Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
  ;; 2. Headers should be read as bytes and then translated to unicode as appropriate.
  ;; 3. The Authorization header should have bytes (i.e. (cdr pass-pair) is bytes
  (define (basic-auth-extract-user-pass headers)
    (match (headers-assq* #"Authorization" headers)
      [#f #f]
      [(struct header (_ basic-credentials))
       (cond
         [(and (regexp-match #rx#"^Basic .*"
                             basic-credentials)
               (regexp-match #rx"([^:]*):(.*)"
                             (base64-decode (subbytes basic-credentials 6 (bytes-length basic-credentials)))))
          => (lambda (user-pass)
               (cons (cadr user-pass) (caddr user-pass)))]
         [else #f])]))
  
  (provide/contract
   ; XXX contract maybe
   [basic-auth-extract-user-pass ((listof header?) . -> . (or/c false/c (cons/c bytes? bytes?)))]   
   [make-html-response/incremental (((string? . -> . void) . -> . void) . -> . response/incremental?)]
   [redirect-to ((string?) (redirection-status?) . opt-> . response/full?)]
   [permanently redirection-status?]
   [temporarily redirection-status?]
   [see-other redirection-status?]))