(module configuration mzscheme
  (require (lib "kw.ss")
           (lib "contract.ss"))
  (require "private/configuration.ss"
           "private/configuration-table-structs.ss"
           "private/util.ss"
           "private/parse-table.ss"
           "web-config-sig.ss")
    
  (define default-configuration-table-path
    (build-path (collection-path "web-server") "configuration-table"))
  
  ; get-configuration : path -> configuration-table
  (define (get-configuration table-file-name)
    (parse-configuration-table (call-with-input-file table-file-name read)))
  
  ; load-configuration : path -> configuration
  (define/kw (load-configuration table-file-name
                                 #:other-keys bct-keys)
    (apply load-configuration-sexpr 
           (call-with-input-file table-file-name read)
           #:web-server-root (directory-part table-file-name)
           bct-keys))
  
  ; load-configuration-sexpr : string? sexp -> configuration
  (define/kw (load-configuration-sexpr sexpr
                                       #:key
                                       [web-server-root (directory-part default-configuration-table-path)]
                                       #:other-keys bct-keys)
    (apply complete-configuration
           web-server-root
           (parse-configuration-table sexpr)
           bct-keys))
    
  (provide load-configuration
           load-configuration-sexpr)
  (provide/contract
   [get-configuration (path-string? . -> . configuration-table?)]
   [default-configuration-table-path path?]))