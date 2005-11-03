(module request-structs mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net"))
  
  ;; the request struct as currently doc'd
  (define-struct request (method uri headers bindings/raw
                                 host-ip host-port client-ip))

  ;; header?: anyd/c -> boolean
  ;; is this a header?
  (define header?
    (cons/c symbol? bytes?))

  ;; bindings? any/c -> boolean
  ;; is this a binding
  (define binding?
    (cons/c symbol? 
            (union string?
                   bytes?)))

  (provide header? binding?)
  (provide/contract
   [struct request ([method symbol?] [uri url?] [headers (listof header?)]
                    [bindings/raw (union (listof binding?) string?)]
                    [host-ip string?] [host-port number?]
                    [client-ip string?])]))