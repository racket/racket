#lang racket/base
(require racket/contract
         (for-syntax racket/base)
         racket/runtime-path
         net/url
         web-server/private/xexpr
         web-server/http/xexpr
         web-server/http/response-structs
         web-server/http/request-structs)

(define (format-stack-trace trace)
  `(pre
    ,@(for/list ([item (in-list trace)])
        (format "~a at:\n  ~a\n"
                (if (car item)
                    (car item)
                    "<unknown procedure>")
                (if (cdr item)
                    (format "line ~a, column ~a, in file ~a"
                            (srcloc-line (cdr item))
                            (srcloc-column (cdr item))
                            (srcloc-source (cdr item)))
                    "<unknown location>")))))

(define-runtime-path default-error-style-sheet
  "../default-web-root/htdocs/error.css")

(define (pretty-exception-response url exn)
  (response/xexpr
   #:code 500
   `(html
     (head
      (title "Servlet Error")
      (link ([rel "stylesheet"] [href "/error.css"])))
     (body
      (div ([class "section"])
           (div ([class "title"]) "Exception")
           (p
            "The application raised an exception with the message:"
            (pre ,(if (exn:pretty? exn)
                      (exn:pretty-xexpr exn)
                      (exn-message exn))))
           (p
            "Stack trace:"
            ,(format-stack-trace
              (continuation-mark-set->context (exn-continuation-marks exn)))))))))


; file-response : nat str str [(cons sym str) ...] -> response
; The server should still start without the files there, so the
; configuration tool still runs.  (Alternatively, find an work around.)
(define (file-response code short text-file . headers)
  (response/full code short
                 (current-seconds) TEXT/HTML-MIME-TYPE
                 headers
                 (list (read-file text-file))))

; servlet-loading-responder : url tst -> response
; This is slightly tricky since the (interesting) content comes from the exception.
(define (servlet-loading-responder url exn)
  ((error-display-handler)
   (format "Servlet (@ ~a) didn't load:\n~a\n" (url->string url) (exn-message exn))
   exn)
  (pretty-exception-response url exn))

; gen-servlet-not-found : str -> url -> response
(define (gen-servlet-not-found file-not-found-file)
  (lambda (url)
    (file-response 404 #"Servlet not found" file-not-found-file)))

; servlet-error-response : url exn -> response
(define (servlet-error-responder url exn)
  ((error-display-handler)
   (format "Servlet (@ ~a) exception:\n~a\n" (url->string url) (exn-message exn))
   exn)
  (pretty-exception-response url exn))

; gen-servlet-responder : str -> url tst -> response
(define (gen-servlet-responder servlet-error-file)
  (lambda (url exn)
    ((error-display-handler)
     (format "Servlet (@ ~a) exception:\n~e\n" (url->string url) (exn-message exn))
     exn)
    (file-response 500 #"Servlet error" servlet-error-file)))

; gen-servlets-refreshed : str -> -> response
(define (gen-servlets-refreshed servlet-refresh-file)
  (lambda ()
    (file-response 200 #"Servlet cache refreshed" servlet-refresh-file)))

; gen-passwords-refreshed : str -> -> response
(define (gen-passwords-refreshed password-refresh-file)
  (lambda ()
    (file-response 200 #"Passwords refreshed" password-refresh-file)))

; gen-authentication-responder : str -> url (cons sym str) -> response
(define (gen-authentication-responder access-denied-file)
  (lambda (uri recommended-header)
    (file-response 401 #"Authorization Required" access-denied-file
                   recommended-header)))

; gen-protocol-responder : str -> str -> response
(define (gen-protocol-responder protocol-file)
  (lambda (error-message)
    (file-response 400 #"Malformed Request" protocol-file)))

; gen-file-not-found-responder : str -> req -> response
(define (gen-file-not-found-responder file-not-found-file)
  (lambda (req)
    (file-response 404 #"File not found" file-not-found-file)))

; gen-collect-garbage-responder : str -> -> response
(define (gen-collect-garbage-responder file)
  (lambda ()
    (file-response 200 #"Garbage collected" file)))

; read-file : str -> str
(define (read-file path)
  (call-with-input-file path
    (lambda (in) (read-bytes (file-size path) in))))

(provide/contract
 [file-response ((natural-number/c bytes? path-string?) () #:rest (listof header?) . ->* . response?)]
 [servlet-loading-responder (url? exn? . -> . response?)]
 [gen-servlet-not-found (path-string? . -> . (url? . -> . response?))]
 [servlet-error-responder (url? exn? . -> . response?)]
 [gen-servlet-responder (path-string? . -> . (url? exn? . -> . response?))]
 [gen-servlets-refreshed (path-string? . -> . (-> response?))]
 [gen-passwords-refreshed (path-string? . -> . (-> response?))]
 [gen-authentication-responder (path-string? . -> . (url? header? . -> . response?))]
 [gen-protocol-responder (path-string? . -> . (url? . -> . response?))]
 [gen-file-not-found-responder (path-string? . -> . (request? . -> . response?))]
 [gen-collect-garbage-responder (path-string? . -> . (-> response?))])
