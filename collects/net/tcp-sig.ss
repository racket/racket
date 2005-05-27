(module tcp-sig mzscheme
  (provide net:tcp^)
  (require (lib "unitsig.ss"))
  
  (define-signature net:tcp^
    (tcp-abandon-port
     tcp-accept
     tcp-accept-ready?
     tcp-addresses
     tcp-close
     tcp-connect
     tcp-connect/enable-break
     tcp-listen
     tcp-listener?)))