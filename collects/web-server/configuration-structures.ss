(module configuration-structures mzscheme
  (require (lib "unitsig.ss")
           (lib "contract.ss")
           (lib "url.ss" "net"))
  (require "response-structs.ss"
           "configuration-table-structs.ss")
  
  ; configuration is now a unit.  See sig.ss
  ; XXX contract
  (define configuration?
    unit/sig?)
  
  ; host = (make-host (listof str) sym string
  ;                   passwords responders timeouts paths)
  (define-struct host (indices log-format log-path passwords responders timeouts paths))  
  
  ; passwords = (listof (list* relm:str protected-dir-regexp:str
  ;                            (listof (list user:sym password:str))))
  
  ; responders = (make-responders (url tst -> response)
  ;                               (url tst -> response)
  ;                               (url (cons sym str) -> response)
  ;                               response
  ;                               response
  ;                               (url -> response)
  ;                               response
  ;                               response)
  (define-struct responders
    (servlet servlet-loading authentication servlets-refreshed passwords-refreshed file-not-found protocol collect-garbage))

  (provide ; all-from
   (struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
   (struct paths (host-base log htdocs mime-types servlet)))
  (provide/contract
   [configuration? (any/c . -> . boolean?)]
   [struct host 
           ([indices (listof string?)]
            [log-format symbol?]
            [log-path (or/c false/c path? string?)]
            [passwords (or/c false/c path? string?)]
            #;[passwords (listof (cons/c string?
                                       (cons/c string?
                                               (listof (list/c symbol? string?)))))]
            [responders responders?]
            [timeouts timeouts?]
            [paths paths?])]
   [struct responders
           ([servlet (url? any/c . -> . response?)]
            [servlet-loading (url? any/c . -> . response?)]
            [authentication (url? (cons/c symbol? string?) . -> . response?)]
            [servlets-refreshed (-> response?)]
            [passwords-refreshed (-> response?)]
            [file-not-found (url? . -> . response?)]
            [protocol (url? . -> . response?)]
            [collect-garbage (-> response?)])]))
