(module url-structs mzscheme
  (require (lib "contract.ss"))
  
  (define-struct url (scheme user host port path-absolute? path query fragment))
  (define-struct path/param (path param))

  (provide/contract
   (struct url ([scheme (union false/c string?)]
                [user (union false/c string?)]
                [host (union false/c string?)]
                [port (union false/c number?)]
                [path-absolute? boolean?]
                [path (listof path/param?)]
                [query (listof (cons/c symbol? string?))]
                [fragment (union false/c string?)]))
   (struct path/param ([path (union string? (symbols 'up 'same))]
                       [param (listof string?)]))))
