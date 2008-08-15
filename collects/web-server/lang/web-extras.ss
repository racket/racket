#lang scheme/base
(require net/url
         (for-template "web.ss")
         "web.ss"
         "../servlet/helpers.ss")
(provide redirect/get)  

(define (redirect/get)
  (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
