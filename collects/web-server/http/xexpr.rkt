#lang racket/base
(require racket/contract
         racket/list
         xml
         web-server/private/xexpr
         unstable/contract
         "request-structs.rkt"
         "cookie.rkt"
         "response-structs.rkt")

(define xexpr-response/c
  (coerce/c
   (λ (x)
     (cond
       [(response? x)
        x]
       [(xexpr? x)
        (response/xexpr x)]
       [else
        #f]))))

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
 [xexpr-response/c contract?]
 [response/xexpr 
  ((pretty-xexpr/c)
   (#:code number? #:message bytes? #:seconds number? #:mime-type bytes? #:headers (listof header?) #:preamble bytes?)
   . ->* . response?)])
