(module tcp-unit mzscheme
  (provide tcp@)
  (require (lib "unitsig.ss")
           "tcp-sig.ss")
  
  ; Okay, this file looks retarded.  Something is clearly wrong.
  
  
  (define raw:tcp-abandon-port tcp-abandon-port)
  (define raw:tcp-accept tcp-accept) 
  (define raw:tcp-accept-ready? tcp-accept-ready?)
  (define raw:tcp-addresses tcp-addresses)
  (define raw:tcp-close tcp-close)
  (define raw:tcp-connect tcp-connect)
  (define raw:tcp-connect/enable-break tcp-connect/enable-break)
  (define raw:tcp-listen tcp-listen)
  (define raw:tcp-listener? tcp-listener?)
  
  (define tcp@
    (unit/sig net:tcp^
      (import)
      
      (define tcp-abandon-port raw:tcp-abandon-port)
      (define tcp-accept raw:tcp-accept) 
      (define tcp-accept-ready? raw:tcp-accept-ready?)
      (define tcp-addresses raw:tcp-addresses)
      (define tcp-close raw:tcp-close)
      (define tcp-connect raw:tcp-connect)
      (define tcp-connect/enable-break raw:tcp-connect/enable-break)
      (define tcp-listen raw:tcp-listen)
      (define tcp-listener? raw:tcp-listener?)
      )))