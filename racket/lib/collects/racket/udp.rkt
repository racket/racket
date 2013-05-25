
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
             udp-send-to-evt
             udp-addresses
	     udp-multicast-loopback?
	     udp-multicast-set-loopback!
	     udp-multicast-ttl
	     udp-multicast-set-ttl!
	     udp-multicast-interface
	     udp-multicast-set-interface!
	     udp-multicast-join-group!
	     udp-multicast-leave-group!)
      
  (define-values (udp-addresses)
    (case-lambda
      [(x) (udp-addresses x #f)]
      [(socket port-numbers?)
        (if (udp? socket)
            (tcp-addresses socket port-numbers?)
            (raise-argument-error 'udp-addresses "udp?" socket))])))
