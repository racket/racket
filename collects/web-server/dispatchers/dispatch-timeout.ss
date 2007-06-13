(module dispatch-timeout mzscheme
  (require (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/connection-manager.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make (integer? . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make new-timeout) conn req)
    (adjust-connection-timeout! conn new-timeout)
    (next-dispatcher)))