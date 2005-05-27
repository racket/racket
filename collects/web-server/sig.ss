(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide web-server^ servlet^ web-config^ web-config/pervasive^ web-config/local^)

  (define-signature web-server^
    (serve
     serve-ports
     ; for environment:
     server-loop))
  
  (define-signature servlet^
    (initial-request send/suspend send/finish send/back send/forward adjust-timeout!))

  ; more here - rename
  (define-signature web-config/pervasive^
    (max-waiting 
     virtual-hosts
     access
     scripts ;; : equal-hash-table
     initial-connection-timeout))

  ; more here - rename
  (define-signature web-config/local^ (port listen-ip instances make-servlet-namespace))

  (define-signature web-config^
    ((open web-config/pervasive^) (open web-config/local^))))