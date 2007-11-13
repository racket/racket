
(module tcp '#%kernel
  (#%require '#%network)

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
             tcp-port?))
