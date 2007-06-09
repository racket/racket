(module request-structs mzscheme
  (require (lib "contract.ss")
           (lib "serialize.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net"))
  
  (define (bytes-ci=? b0 b1)
    (string-ci=? (bytes->string/utf-8 b0)
                 (bytes->string/utf-8 b1)))
  
  (define-serializable-struct header (field value))
  (define (headers-assq* f hs)
    (match hs
      [(list)
       #f]
      [(list-rest (and h (struct header (af aw))) hs)
       (if (bytes-ci=? af f)
           h
           (headers-assq f hs))]))
  (define (headers-assq f hs)
    (match hs
      [(list)
       #f]
      [(list-rest (and h (struct header (af av))) hs)
       (if (bytes=? af f)
           h
           (headers-assq f hs))]))       
  (provide/contract
   [headers-assq (bytes? (listof header?) . -> . (or/c false/c header?))]
   [headers-assq* (bytes? (listof header?) . -> . (or/c false/c header?))]
   [struct header ([field bytes?]
                   [value bytes?])])
  
  (define-serializable-struct binding (id))
  (define-serializable-struct (binding:form binding) (value))
  (define-serializable-struct (binding:file binding) (filename content))
  (define (bindings-assq ti bs)
    (match bs
      [(list)
       #f]
      [(list-rest (and b (struct binding (i))) bs)
       (if (equal? ti i)
           b
           (bindings-assq ti bs))]))
  (provide/contract
   [bindings-assq (bytes? (listof binding?) . -> . (or/c false/c binding?))]
   [struct binding ([id bytes?])]
   [struct (binding:form binding) ([id bytes?]
                                   [value bytes?])]
   [struct (binding:file binding) ([id bytes?]
                                   [filename bytes?]
                                   [content bytes?])])
  
  (define-serializable-struct request (method uri headers/raw bindings/raw post-data/raw
                                              host-ip host-port client-ip))
  (provide/contract
   [struct request ([method symbol?]
                    [uri url?] 
                    [headers/raw (listof header?)]
                    [bindings/raw (listof binding?)]
                    [post-data/raw (or/c false/c bytes?)]
                    [host-ip string?] [host-port number?]
                    [client-ip string?])]))