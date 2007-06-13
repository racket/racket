(module response-structs mzscheme
  (require (lib "contract.ss")
           (lib "xml.ss" "xml")
           "request-structs.ss")
  
  (define TEXT/HTML-MIME-TYPE #"text/html; charset=utf-8")
  
  (define-struct response/basic (code message seconds mime headers))
  (define-struct (response/full response/basic) (body))
  (define-struct (response/incremental response/basic) (generator))
    
  ; response = (cons string (listof string)), where the first string is a mime-type
  ;          | x-expression
  ;          | response/basic
  
  ;; response?: any -> boolean
  ;; Determine if an object is a response
  (define (response? x)
    (or (response/basic? x)
        (and (pair? x) (andmap (lambda (e)
                                 (or (string? e)
                                     (bytes? e)))
                               x))
        (xexpr? x)))
  
  (provide/contract
   [struct response/basic
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [headers (listof header?)])]            
   [struct (response/full response/basic)
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [headers (listof header?)]
            [body (listof (or/c string?
                                 bytes?))])]
   [struct (response/incremental response/basic)
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [headers (listof header?)]
            [generator ((() (listof (or/c bytes? string?)) . ->* . any) . -> . any)])]
   [response? (any/c . -> . boolean?)]
   [TEXT/HTML-MIME-TYPE bytes?]))