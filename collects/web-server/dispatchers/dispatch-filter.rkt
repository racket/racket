#lang racket/base
(require racket/contract
         net/url)
(require web-server/dispatchers/dispatch
         web-server/http
         web-server/private/util)
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (regexp? dispatcher/c . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make regex inner) conn req)
  (define path (url-path->string (url-path (request-uri req))))
  #;(printf "~S\n" `(filter ,regex ,(url->string (request-uri req)) ,path ,(regexp-match regex path)))
  (if (regexp-match regex path)
      (inner conn req)
      (next-dispatcher)))
