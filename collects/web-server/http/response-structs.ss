#lang scheme/base
(require mzlib/contract
         xml/xml
         scheme/pretty
         web-server/http/request-structs)

(define TEXT/HTML-MIME-TYPE #"text/html; charset=utf-8")

(define-struct response/basic (code message seconds mime headers))
(define-struct (response/full response/basic) (body))
(define-struct (response/incremental response/basic) (generator))

; Response Contracts
(define xexpr/c 
  (make-proj-contract
   'xexpr?
   (lambda (pos neg src-info name)
     (lambda (val)
       (with-handlers ([exn:invalid-xexpr?
                        (lambda (exn)
                          (raise-contract-error
                           val
                           src-info
                           pos
                           name
                           "Not an Xexpr. ~a~n~nContext:~n~a"
                           (exn-message exn)
                           (pretty-format val)))])
         (validate-xexpr val)
         val)))
   (lambda (v) #t)))

; response = (cons string (listof string)), where the first string is a mime-type
;          | x-expression
;          | response/basic
(define response?
  (or/c response/basic?
        (listof (or/c string? bytes?))
        xexpr/c))

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
 [response? contract?]
 [TEXT/HTML-MIME-TYPE bytes?])
