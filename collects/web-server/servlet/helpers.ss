(module helpers mzscheme
  (require (lib "contract.ss")
           (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "uri-codec.ss" "net"))
  (require "../private/util.ss"
           "bindings.ss"
           "basic-auth.ss"
           "../request-structs.ss"
           "../response-structs.ss")
  (provide (all-from "bindings.ss")
           (all-from "basic-auth.ss")
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
  (define/kw (redirect-to uri
                          #:optional
                          [perm/temp permanently]
                          #:key
                          [headers (list)])
    (make-response/full (redirection-status-code perm/temp)
                        (redirection-status-message perm/temp)
                        (current-seconds) #"text/html"
                        `((Location . ,uri) ,@headers) (list)))
  
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
  
  (provide ; all-from
   with-errors-to-browser
   redirect-to
   (rename uri-decode translate-escapes))
  (provide/contract
   [permanently redirection-status?]
   [temporarily redirection-status?]
   [see-other redirection-status?]
   [request-bindings (request? . -> . (listof (or/c (cons/c symbol? string?)
                                                    (cons/c symbol? bytes?))))]
   [request-headers (request? . -> . (listof (cons/c symbol? string?)))]))