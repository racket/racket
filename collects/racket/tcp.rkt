
(module tcp '#%kernel
  (#%require (all-except '#%network tcp-addresses)
             (rename '#%network c:tcp-addresses tcp-addresses))

  (#%provide tcp-connect 
             tcp-connect/enable-break 
             tcp-listen 
             tcp-close 
             tcp-accept-ready? 
             tcp-accept 
             tcp-accept-evt 
             tcp-accept/enable-break 
             tcp-listener? 
             tcp-addresses 
             tcp-abandon-port 
             tcp-port?)
      
  (define-values (tcp-addresses) 
    (case-lambda
      [(socket) (tcp-addresses socket #f)]
      [(socket port-numbers?) 
        (if (tcp-port? socket)
          (c:tcp-addresses socket port-numbers?)
          (if (tcp-listener? socket)
              (c:tcp-addresses socket port-numbers?)
              (raise-argument-error 'tcp-addresses "(or/c tcp-port? tcp-listener?)" socket)))])))

