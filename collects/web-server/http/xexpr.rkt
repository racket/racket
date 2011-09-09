#lang racket/base
(require racket/contract
         racket/list
         xml
         web-server/private/xexpr
         net/cookie
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
  (response
   code message seconds mime-type 
   ; rfc2109 also recommends some cache-control stuff here for cookies
   (append hdrs (map cookie->header cooks))
   (λ (out)
     (write-bytes preamble out)
     (write-xexpr xexpr out))))

(provide/contract
 [response/xexpr 
  ((pretty-xexpr/c)
   (#:code number? #:message bytes? #:seconds number? #:mime-type (or/c false/c bytes?) #:cookies (listof cookie?) #:headers (listof header?) #:preamble bytes?)
   . ->* . response?)])
