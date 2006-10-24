(module configuration-table-structs mzscheme
  (require (lib "contract.ss"))
  
  ; configuration-table = (make-configuration-table nat nat num host-table (listof (cons str host-table)))
  (define-struct configuration-table
    (port max-waiting initial-connection-timeout default-host virtual-hosts))
  
  ; host-table = (make-host-table (listof str) sym messages timeouts paths)
  (define-struct host-table (indices log-format messages timeouts paths))
  
  ; messages = (make-messages str^6)
  (define-struct messages
    (servlet authentication servlets-refreshed passwords-refreshed file-not-found protocol collect-garbage))

  ; timeouts = (make-timeouts nat^5)
  (define-struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
  
  ; paths = (make-paths str^6)
  (define-struct paths (conf host-base log htdocs servlet mime-types passwords))
  
  (provide/contract
   [struct configuration-table
           ([port natural-number/c]
            [max-waiting natural-number/c]
            [initial-connection-timeout natural-number/c]
            [default-host host-table?]
            [virtual-hosts (listof (cons/c string? host-table?))])]
   [struct host-table 
           ([indices (listof string?)]
            [log-format symbol?]
            [messages messages?]
            [timeouts timeouts?]
            [paths paths?])]
   [struct messages
           ([servlet string?]
            [authentication string?]
            [servlets-refreshed string?]
            [passwords-refreshed string?]
            [file-not-found string?]
            [protocol string?]
            [collect-garbage string?])]
   [struct timeouts 
           ([default-servlet number?]
            [password number?]
            [servlet-connection number?]
            [file-per-byte number?]
            [file-base number?])]
   [struct paths 
           ([conf (or/c false/c path-string?)]
            [host-base (or/c false/c path-string?)]
            [log (or/c false/c path-string?)]
            [htdocs (or/c false/c path-string?)]
            [servlet (or/c false/c path-string?)]
            [mime-types (or/c false/c path-string?)]
            [passwords (or/c false/c path-string?)])]))
