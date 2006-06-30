(module servlet-structs mzscheme
  (require (lib "contract.ss")
           (lib "xml.ss" "xml"))
  (require "request-structs.ss"
           "response-structs.ss")  
  
  (define servlet-response?
    any/c)
  
  (define (xexpr/callback? x)
    (correct-xexpr? x 
                    (lambda () #t)
                    (lambda (exn)
                      (if (procedure? (exn:invalid-xexpr-code exn))
                          #t
                          (begin ((error-display-handler) (exn-message exn) exn)
                                 #f)))))
  
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
   [xexpr/callback? (any/c . -> . boolean?)]
   [response-generator? contract?]
   [k-url? (any/c . -> . boolean?)]
   [url-transform? contract?]
   [expiration-handler? contract?]
   [embed/url? contract?]))