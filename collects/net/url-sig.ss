(module url-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide net:url^)
  
  (define-signature net:url^
    (get-pure-port
     get-impure-port
     post-pure-port
     post-impure-port
     display-pure-port
     purify-port
     netscape/string->url
     string->url
     url->string
     call/input-url
     combine-url/relative
     url-exception?
     current-proxy-servers)))

