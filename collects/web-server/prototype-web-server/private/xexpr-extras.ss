(module xexpr-extras mzscheme
  (require (lib "url.ss" "net")
           (lib "plt-match.ss"))  
  (provide xexpr+extras->xexpr)
  
  (define xexpr+extras->xexpr
    (match-lambda
      [(list xe ...)
       (map xexpr+extras->xexpr xe)]
      [(and url (? url?))
       (url->string url)]
      [x
       x])))
    