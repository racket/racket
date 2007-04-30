(module sig mzscheme
  (require (lib "unit.ss"))
  (require "private/dispatch-server-sig.ss")
  (provide ; XXX contract signature
   (rename dispatch-server^ web-server^) 
   web-config^ web-config/pervasive^ web-config/local^)  

  ; more here - rename
  (define-signature web-config/pervasive^
    (max-waiting 
     virtual-hosts
     access
     scripts
     initial-connection-timeout))

  ; more here - rename
  (define-signature web-config/local^
    (port listen-ip instances make-servlet-namespace))

  (define-signature web-config^
    ((open web-config/pervasive^) (open web-config/local^))))