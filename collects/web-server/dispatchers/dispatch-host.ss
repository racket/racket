(module dispatch-host mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net")
           "../private/request-structs.ss"
           "dispatch.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make ((symbol? . -> . dispatcher?) . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make lookup-dispatcher) conn req)
    (define host (get-host (request-uri req) (request-headers/raw req)))
    ((lookup-dispatcher host) conn req))
  
  ;; get-host : Url (listof (cons Symbol String)) -> Symbol
  ;; XXX host names are case insesitive---Internet RFC 1034
  (define (get-host uri headers)
    (cond
      [(url-host uri) => string->symbol]
      [(headers-assq* #"Host" headers)
       => (match-lambda
            [(struct header (_ v))
             (string->symbol (bytes->string/utf-8 v))])]
      [else '<none>])))