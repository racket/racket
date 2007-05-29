(module sig mzscheme
  (require (lib "unit.ss"))
  (require "private/dispatch-server-sig.ss")
  (provide ; XXX contract signature
   (rename dispatch-server^ web-server^) 
   web-config^)  

  (define-signature web-config^
    (max-waiting 
     virtual-hosts
     access
     scripts
     initial-connection-timeout
     port
     listen-ip
     instances
     make-servlet-namespace)))