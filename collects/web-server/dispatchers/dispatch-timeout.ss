#lang scheme/base
(require mzlib/contract)
(require "dispatch.ss"
         "../private/connection-manager.ss")
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (integer? . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make new-timeout) conn req)
  (adjust-connection-timeout! conn new-timeout)
  (next-dispatcher))
