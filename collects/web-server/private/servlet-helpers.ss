(module servlet-helpers mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "plt-match.ss")
           (lib "base64.ss" "net")
           (lib "uri-codec.ss" "net"))
  (require "util.ss"
           "bindings.ss"
           "../request-structs.ss"
           "../response-structs.ss")
  (provide (all-from "bindings.ss")
           (all-from "../response-structs.ss")
           (all-from "../request-structs.ss"))
  
  (define (request-headers request)
    (map (match-lambda
           [(struct header (field value))
            (cons (lowercase-symbol! (bytes->string/utf-8 field))
                  (bytes->string/utf-8 value))])
         (request-headers/raw request)))
  (define (request-bindings request)
    (map (match-lambda
           [(struct binding:form (id value))
            (cons (lowercase-symbol! (bytes->string/utf-8 id))
                  (bytes->string/utf-8 value))]
           [(struct binding:file (id fname value))
            (cons (lowercase-symbol! (bytes->string/utf-8 id))
                  value)])
         (request-bindings/raw request)))
  
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
  
  ; with-errors-to-browser 
  ; to report exceptions that occur later to the browser
  ; this must be called at the begining of a servlet
  (define (with-errors-to-browser send/finish-or-back thunk)
    (with-handlers ([exn? (lambda (exn)
                            (send/finish-or-back
                             `(html (head (title "Servlet Error"))
                                    (body ([bgcolor "white"])
                                          (p "The following error occured: "
                                             (pre ,(exn->string exn)))))))])
      (thunk)))
  
  ; Authentication  
  (define AUTHENTICATION-REGEXP (regexp "([^:]*):(.*)"))
  (define (match-authentication x) (regexp-match AUTHENTICATION-REGEXP x))
  ;:(define match-authentication (type: (str -> (or/c false (list str str str)))))
  
  ; extract-user-pass : (listof (cons sym bytes)) -> (or/c #f (cons str str))
  ;; Notes (GregP)
  ;; 1. This is Basic Authentication (RFC 1945 SECTION 11.1)
  ;;    e.g. an authorization header will look like this:
  ;;         Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
  ;; 2. Headers should be read as bytes and then translated to unicode as appropriate.
  ;; 3. The Authorization header should have bytes (i.e. (cdr pass-pair) is bytes
  (define (extract-user-pass headers)
    (match (headers-assq* #"Authorization" headers)
      [#f #f]
      [(struct header (_ basic-credentials))
       (cond
         [(and (basic? basic-credentials)
               (match-authentication
                (base64-decode (subbytes basic-credentials 6 (bytes-length basic-credentials)))))
          => (lambda (user-pass)
               (cons (cadr user-pass) (caddr user-pass)))]
         [else #f])]))
  
  ;; basic?: bytes -> (or/c (listof bytes) #f)
  ;; does the second part of the authorization header start with #"Basic "
  (define basic?
    (let ([rx (byte-regexp #"^Basic .*")])
      (lambda (a) (regexp-match rx a))))
  
  (provide ; all-from
   with-errors-to-browser
   (rename uri-decode translate-escapes))
  (provide/contract
   ; XXX contract maybe   
   [extract-user-pass ((listof header?) . -> . (or/c false/c (cons/c bytes? bytes?)))]
   [redirect-to ((string?) (redirection-status?) . opt-> . response/full?)]
   [permanently redirection-status?]
   [temporarily redirection-status?]
   [see-other redirection-status?]
   [request-bindings (request? . -> . (listof (or/c (cons/c symbol? string?)
                                                    (cons/c symbol? bytes?))))]
   [request-headers (request? . -> . (listof (cons/c symbol? string?)))]))