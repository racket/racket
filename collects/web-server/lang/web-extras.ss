#lang scheme/base
(require net/url
         scheme/contract
         "web.ss"
         web-server/http)
(provide/contract
 [redirect/get (-> request?)])  

(define (redirect/get)
  (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
