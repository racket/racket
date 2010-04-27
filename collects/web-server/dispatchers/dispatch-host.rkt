#lang scheme/base
(require mzlib/contract
         mzlib/plt-match
         net/url
         web-server/http
         "../private/util.ss"
         "dispatch.ss")
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make ((symbol? . -> . dispatcher/c) . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make lookup-dispatcher) conn req)
  (define host (get-host (request-uri req) (request-headers/raw req)))
  ((lookup-dispatcher host) conn req))

;; get-host : Url (listof (cons Symbol String)) -> Symbol
(define (get-host uri headers)
  (cond
    [(url-host uri) 
     => lowercase-symbol!]
    [(headers-assq* #"Host" headers)
     => (match-lambda
          [(struct header (_ v))
           (lowercase-symbol! v)])]
    [else 'none]))
