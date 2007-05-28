(module dispatch-filter mzscheme
  (require (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/util.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make (regexp? dispatcher? . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make regex inner) conn req)
    (define-values (uri method path) (decompose-request req))
    (if (regexp-match regex path)
        (inner conn req)
        (next-dispatcher))))