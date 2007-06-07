(module basic-auth mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "base64.ss" "net"))
  (require "../private/request-structs.ss")
  
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
   [extract-user-pass ((listof header?) . -> . (or/c false/c (cons/c bytes? bytes?)))]))