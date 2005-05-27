(module configuration-table-structs mzscheme
  (require "util.ss")
  
  ; configuration-table = (make-configuration-table nat nat num host-table (listof (cons str host-table)))
  (provide-define-struct
   configuration-table
   (port max-waiting initial-connection-timeout default-host virtual-hosts))
  
  ; host-table = (make-host-table (listof str) sym messages timeouts paths)
  (provide-define-struct host-table (indices log-format messages timeouts paths))
  
  ; passwords = (listof (list* relm:str protected-dir-regexp:str (listof (list user:sym password:str))))
  ; passwords moved back to a separate file
  
  ; messages = (make-messages str^6)
  (provide-define-struct messages
    (servlet ;servlet-loading
     authentication servlets-refreshed passwords-refreshed file-not-found protocol))
  
  ; timeouts = (make-timeouts nat^5)
  (provide-define-struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
  
  ; paths = (make-paths str^6)
  (provide-define-struct paths (conf host-base log htdocs servlet passwords)))
