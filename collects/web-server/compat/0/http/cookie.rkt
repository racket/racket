#lang racket/base
(require (prefix-in new: web-server/http/cookie)
         web-server/http/xexpr)

(define (xexpr-response/cookies cs xe)
  (response/xexpr xe #:cookies cs))

(provide 
 (rename-out
  [new:make-cookie make-cookie]
  [new:cookie->header cookie->header])
 (all-from-out))