#lang scheme/base
(require net/url
         scheme/contract
         (for-template "web.ss")
         "web.ss"
         web-server/private/request-structs
         "../servlet/helpers.ss")
(provide/contract
 [redirect/get (-> request?)])  

(define (redirect/get)
  (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
