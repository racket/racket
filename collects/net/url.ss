(module url mzscheme
  (require (lib "unitsig.ss")
           (lib "contract.ss")
           "url-sig.ss"
           "url-unit.ss"
           "tcp-sig.ss"
           "tcp-unit.ss")

  (define-values/invoke-unit/sig
   net:url^
   (compound-unit/sig
     (import)
     (link
      [T : net:tcp^ (tcp@)]
      [U : net:url^ (url@ T)])
     (export (open U))))

  (provide/contract
   (struct url ([scheme (union false/c string?)]
                [user (union false/c string?)]
                [host (union false/c string?)]
                [port (union false/c number?)]
                [path (listof (union string? path/param?))]
                [query (listof (cons/c symbol? string?))]
                [fragment (union false/c string?)]))
   (struct path/param ([path string?]
                       [param string?]))
   (string->url ((union bytes? string?) . -> . url?))
   (url->string (url? . -> . string?))

   (get-pure-port (opt-> (url?) ((listof string?)) input-port?))
   (get-impure-port (opt-> (url?) ((listof string?)) input-port?))
   (post-pure-port (opt-> (url? (union false/c bytes?)) ((listof string?)) input-port?))
   (post-impure-port (opt-> (url? bytes?) ((listof string?)) input-port?))
   (display-pure-port (input-port? . -> . void?))
   (purify-port (input-port? . -> . string?))
   (netscape/string->url (string? . -> . url?))
   (call/input-url (opt->* (url?
			    (opt-> (url?) ((listof string?)) input-port?)
			    (input-port? . -> . any))
			   ((listof string?))
			   any))
   (combine-url/relative (url? string? . -> . url?))
   (url-exception? (any/c . -> . boolean?))
   (current-proxy-servers
    (case-> ((union false/c (listof (list/c string? string? number?))) . -> . void?)
            (-> (union false/c (listof (list/c string? string? number?))))))))

