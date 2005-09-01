(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide 
   dispatch-server^ dispatch-server-config^
   web-server^ servlet^ web-config^ web-config/pervasive^ web-config/local^)

  (define-signature dispatch-server^
    (serve
     serve-ports
     ; for environment:
     server-loop))
  (define-signature web-server^
    ((open dispatch-server^)))
  
  (define-signature dispatch-server-config^
    (port listen-ip max-waiting initial-connection-timeout
          read-request dispatch))
  
  (define-signature servlet^
    (initial-request send/suspend send/finish send/back send/forward adjust-timeout!))

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