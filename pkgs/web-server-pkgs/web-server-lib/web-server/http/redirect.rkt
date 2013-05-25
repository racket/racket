#lang racket/base
(require racket/contract
         web-server/private/util
         web-server/http/response-structs
         web-server/http/request-structs)

; redirection-status = (make-redirection-status nat bytes)
(define-struct redirection-status (code message))

(define permanently (make-redirection-status 301 #"Moved Permanently"))
(define temporarily (make-redirection-status 302 #"Moved Temporarily"))
(define see-other (make-redirection-status 303 #"See Other"))

; : str [redirection-status] -> response
(define (redirect-to 
         uri
         [perm/temp temporarily]
         #:headers [headers (list)])
  (response (redirection-status-code perm/temp)
            (redirection-status-message perm/temp)
            (current-seconds) #"text/html"
            (list* (make-header #"Location" (string->bytes/utf-8 uri))
                   headers)
            void))

(provide/contract
 [redirect-to
  (->* (non-empty-string?) (redirection-status? #:headers (listof header?))
       response?)]
 [redirection-status? (any/c . -> . boolean?)]
 [permanently redirection-status?]
 [temporarily redirection-status?]
 [see-other redirection-status?])
