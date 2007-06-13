(module helpers mzscheme
  (require (lib "contract.ss")
           (lib "kw.ss"))
  (require "../private/util.ss"
           "../private/request-structs.ss"
           "../private/response-structs.ss")
    
  ; redirection-status = (make-redirection-status nat str)
  (define-struct redirection-status (code message))
  
  (define permanently (make-redirection-status 301 "Moved Permanently"))
  (define temporarily (make-redirection-status 302 "Moved Temporarily"))
  (define see-other (make-redirection-status 303 "See Other"))
  
  ; : str [redirection-status] -> response
  (define/kw (redirect-to uri
                          #:optional
                          [perm/temp temporarily]
                          #:key
                          [headers (list)])
    (make-response/full (redirection-status-code perm/temp)
                        (redirection-status-message perm/temp)
                        (current-seconds) #"text/html"
                        (list* (make-header #"Location" (string->bytes/utf-8 uri))
                               headers)
                        (list)))
  
  (define (with-errors-to-browser send/finish-or-back thunk)
    (with-handlers ([exn? (lambda (exn)
                            (send/finish-or-back
                             `(html (head (title "Servlet Error"))
                                    (body ([bgcolor "white"])
                                          (p "The following error occured: "
                                             (pre ,(exn->string exn)))))))])
      (thunk)))
  
  (provide
   with-errors-to-browser
   redirect-to)
  (provide/contract
   [redirection-status? (any/c . -> . boolean?)]
   [permanently redirection-status?]
   [temporarily redirection-status?]
   [see-other redirection-status?]))