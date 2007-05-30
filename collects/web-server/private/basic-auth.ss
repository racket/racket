(module basic-auth mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "base64.ss" "net"))
  (require "../request-structs.ss")
  
  ; Authentication  
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
               (regexp-match #rx"([^:]*):(.*)"
                (base64-decode (subbytes basic-credentials 6 (bytes-length basic-credentials)))))
          => (lambda (user-pass)
               (cons (cadr user-pass) (caddr user-pass)))]
         [else #f])]))
  
  ;; basic?: bytes -> (or/c (listof bytes) #f)
  ;; does the second part of the authorization header start with #"Basic "
  (define basic?
    (let ([rx (byte-regexp #"^Basic .*")])
      (lambda (a) (regexp-match rx a))))
  
  (provide/contract
   ; XXX contract maybe   
   [extract-user-pass ((listof header?) . -> . (or/c false/c (cons/c bytes? bytes?)))]))