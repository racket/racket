#lang racket/base
(require racket/contract)
(require web-server/dispatchers/dispatch
         web-server/private/connection-manager)
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (integer? . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make new-timeout) conn req)
  (adjust-connection-timeout! conn new-timeout)
  (next-dispatcher))
