#lang racket/base
(require racket/contract
         racket/list
         xml
         web-server/private/xexpr
         "request-structs.rkt"
         "cookie.rkt"
         "response-structs.rkt")

(define (response/xexpr
         xexpr
         #:code [code 200] 
         #:message [message #"Okay"]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
         #:cookies [cooks empty]
         #:headers [hdrs empty]
         #:preamble [preamble #""])
  (response/full 
   code message seconds mime-type 
   ; rfc2109 also recommends some cache-control stuff here for cookies
   (append hdrs (map cookie->header cooks))
   ; XXX Use a normal response and an efficient xexpr printer
   (list preamble (string->bytes/utf-8 (xexpr->string xexpr)))))

(provide/contract
 [response/xexpr 
  ((pretty-xexpr/c)
   (#:code number? #:message bytes? #:seconds number? #:mime-type bytes? #:headers (listof header?) #:preamble bytes?)
   . ->* . response?)])