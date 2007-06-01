(module dispatch-filter mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net"))
  (require "dispatch.ss"
           "../private/request-structs.ss"
           "../private/util.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make (regexp? dispatcher? . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make regex inner) conn req)
    (define path (url-path->string (url-path (request-uri req))))
    #;(printf "~S~n" `(filter ,regex ,(url->string (request-uri req)) ,path ,(regexp-match regex path)))
    (if (regexp-match regex path)
        (inner conn req)
        (next-dispatcher))))