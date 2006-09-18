; The main program of the "web-server-monitor" launcher. 
(module monitor-launch mzscheme
  (require "monitor-server.ss"
           "util.ss"
           (lib "cmdline.ss"))
  
  ; handle-numeric-flag : sym -> str str -> (cons sym num)
  (define (handle-numeric-flag name)
    (lambda (dc-flag arg)
      (cons name (string->number arg))))
  
  (parse-command-line
   "web-server-monitor"
   (current-command-line-arguments)
   `((once-each
      [("-p" "--port")
       ,(handle-numeric-flag 'port)
       ("Connects to the network port <port>." "port")]
      [("-f" "--frequency")
       ,(handle-numeric-flag 'frequency)
       ("Polls every <frequency> seconds." "frequency")]
      [("-t" "--timeout")
       ,(handle-numeric-flag 'timeout)
       ("Assumes failure after <timeout> seconds." "timeout")]))
   (lambda (flags email-address host-name)
     (monitor email-address
              host-name
              (extract-flag 'port flags default-server-port)
              (extract-flag 'frequency flags default-poll-frequency-seconds)
              (extract-flag 'timeout flags default-server-response-timeout-seconds)))
   '("email-address" "host-name")))
