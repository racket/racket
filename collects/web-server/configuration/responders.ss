(module responders mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "url.ss" "net"))
  (require "../private/response-structs.ss"
           "../private/request-structs.ss")
  
  ; file-response : nat str str [(cons sym str) ...] -> response
  ; The server should still start without the files there, so the
  ; configuration tool still runs.  (Alternatively, find an work around.)
  (define (file-response code short text-file . headers)
    (make-response/full code short
                        (current-seconds) TEXT/HTML-MIME-TYPE
                        headers
                        (list (read-file text-file))))
  
  ; servlet-loading-responder : url tst -> response
  ; This is slightly tricky since the (interesting) content comes from the exception.
  (define (servlet-loading-responder url exn)
    ((error-display-handler)
       (format "Servlet didn't load:\n~a\n" (exn-message exn))
       exn)
    (make-response/full 500 "Servlet didn't load"
                        (current-seconds)
                        TEXT/HTML-MIME-TYPE
                        empty
                        (list "Servlet didn't load.\n")))
  
  ; gen-servlet-not-found : str -> url -> response
  (define (gen-servlet-not-found file-not-found-file)
    (lambda (url)
      (file-response 404 "Servlet not found" file-not-found-file)))
  
  ; gen-servlet-responder : str -> url tst -> response
  (define (gen-servlet-responder servlet-error-file)
    (lambda (url exn)
      ((error-display-handler)
       (format "Servlet exception:\n~a\n" (exn-message exn))
       exn)
      (file-response 500 "Servlet error" servlet-error-file)))
  
  ; gen-servlets-refreshed : str -> -> response
  (define (gen-servlets-refreshed servlet-refresh-file)
    (lambda ()
      (file-response 200 "Servlet cache refreshed" servlet-refresh-file)))
  
  ; gen-passwords-refreshed : str -> -> response
  (define (gen-passwords-refreshed password-refresh-file)
    (lambda ()
      (file-response 200 "Passwords refreshed" password-refresh-file)))
  
  ; gen-authentication-responder : str -> url (cons sym str) -> response
  (define (gen-authentication-responder access-denied-file)
    (lambda (uri recommended-header)
      (file-response 401 "Authorization Required" access-denied-file
                     recommended-header)))
  
  ; gen-protocol-responder : str -> str -> response
  (define (gen-protocol-responder protocol-file)
    (lambda (error-message)
      (file-response 400 "Malformed Request" protocol-file)))
  
  ; gen-file-not-found-responder : str -> req -> response
  (define (gen-file-not-found-responder file-not-found-file)
    (lambda (req)
      (file-response 404 "File not found" file-not-found-file)))
  
  ; gen-collect-garbage-responder : str -> -> response
  (define (gen-collect-garbage-responder file)
    (lambda ()
      (file-response 200 "Garbage collected" file)))
  
  ; read-file : str -> str
  (define (read-file path)
    (call-with-input-file path
      (lambda (in) (read-string (file-size path) in))))
  
  (provide/contract
   [file-response ((natural-number/c string? path-string?) (listof header?) . ->* . (response?))]
   [servlet-loading-responder (url? any/c . -> . response?)]
   [gen-servlet-not-found (path-string? . -> . (url? . -> . response?))]
   [gen-servlet-responder (path-string? . -> . (url? any/c . -> . response?))]
   [gen-servlets-refreshed (path-string? . -> . (-> response?))]
   [gen-passwords-refreshed (path-string? . -> . (-> response?))]
   [gen-authentication-responder (path-string? . -> . (url? header? . -> . response?))]
   [gen-protocol-responder (path-string? . -> . (url? . -> . response?))]
   [gen-file-not-found-responder (path-string? . -> . (request? . -> . response?))]
   [gen-collect-garbage-responder (path-string? . -> . (-> response?))]))