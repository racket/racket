(module servlet-structs mzscheme
  (require (lib "contract.ss")
           (lib "xml.ss" "xml"))
  (require "../private/request-structs.ss"
           "../private/response-structs.ss")  
  
  (define servlet-response?
    any/c)
    
  (define k-url?
    string?)

  (define response-generator?
    (k-url? . -> . servlet-response?))
  
  (define url-transform?
    (k-url? . -> . k-url?))
  
  (define expiration-handler?
    (or/c false/c
          (request? . -> . response?)))
    
  (define embed/url?
    (((request? . -> . any/c)) (expiration-handler?) . opt-> . string?))
  
  (provide/contract
   [servlet-response? contract?]
   [response-generator? contract?]
   [k-url? (any/c . -> . boolean?)]
   [url-transform? contract?]
   [expiration-handler? contract?]
   [embed/url? contract?]))