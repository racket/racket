(module url-structs mzscheme
  (require (lib "contract.ss"))

  (define-struct url (scheme user host port path-absolute? path query fragment))
  (define-struct path/param (path param))

  (provide/contract
   (struct url ([scheme (or/c false/c string?)]
                [user (or/c false/c string?)]
                [host (or/c false/c string?)]
                [port (or/c false/c number?)]
                [path-absolute? boolean?]
                [path (listof path/param?)]
                [query (listof (cons/c symbol? string?))]
                [fragment (or/c false/c string?)]))
   (struct path/param ([path (or/c string? (symbols 'up 'same))]
                       [param (listof string?)]))))
