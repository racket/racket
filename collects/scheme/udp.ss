
(module udp '#%kernel
  (#%require '#%network)

  (#%provide udp-open-socket 
             udp-close 
             udp? 
             udp-bound? 
             udp-connected? 
             udp-bind! 
             udp-connect! 
             udp-send-to 
             udp-send 
             udp-send-to* 
             udp-send* 
             udp-send-to/enable-break 
             udp-send/enable-break 
             udp-receive! 
             udp-receive!* 
             udp-receive!/enable-break 
             udp-receive-ready-evt 
             udp-send-ready-evt 
             udp-receive!-evt 
             udp-send-evt 
             udp-send-to-evt))
