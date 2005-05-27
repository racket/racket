(module configuration-structures mzscheme
  (require "util.ss"
           "configuration-table-structs.ss")
  (provide (struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
           (struct paths (host-base log htdocs servlet)))

  ; configuration is now a unit.  See sig.ss

  ; host = (make-host (listof str) (str str sym url str -> str)
  ;                   passwords resopnders timeouts paths)
  (provide-define-struct
   host (indices log-message passwords responders timeouts paths))

  ; passwords = (listof (list* relm:str protected-dir-regexp:str
  ;                            (listof (list user:sym password:str))))

  ; responders = (make-responders (url tst -> response)
  ;                               (url tst -> response)
  ;                               (url (cons sym str) -> response)
  ;                               response
  ;                               response
  ;                               (url -> response)
  ;                               response)
  (provide-define-struct
   responders
   (servlet servlet-loading authentication servlets-refreshed passwords-refreshed file-not-found protocol)))
