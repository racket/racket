(module servlet-structs mzscheme
  (require (lib "contract.ss"))
  (require "../private/request-structs.ss"
           "../private/response-structs.ss")  
      
  (define k-url?
    string?)

  (define response-generator?
    (k-url? . -> . response?))
  
  (define url-transform?
    (k-url? . -> . k-url?))
  
  (define expiration-handler?
    (or/c false/c
          (request? . -> . response?)))
    
  (define embed/url?
    (((request? . -> . any/c)) (expiration-handler?) . opt-> . string?))
  
  (provide/contract
   [response-generator? contract?]
   [k-url? (any/c . -> . boolean?)]
   [url-transform? contract?]
   [expiration-handler? contract?]
   [embed/url? contract?]))