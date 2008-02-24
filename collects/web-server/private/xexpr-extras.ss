#lang scheme/base
(require net/url
         mzlib/plt-match)  
(provide xexpr+extras->xexpr)

(define xexpr+extras->xexpr
  (match-lambda
    [(list xe ...)
     (map xexpr+extras->xexpr xe)]
    [(and url (? url?))
     (url->string url)]
    [x
     x]))
