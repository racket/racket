(module monitor-server mzscheme
  (require (lib "etc.ss")
           "monitor-poke-web-server.ss"
           "monitor-emailer.ss"
           (lib "match.ss"))
  
  (provide monitor
           default-server-port
           default-poll-frequency-seconds
           default-server-response-timeout-seconds)
  
  (define default-server-port 80)
  (define default-poll-frequency-seconds 3600)
  (define default-server-response-timeout-seconds 75)
  
  ; monitor : str str [nat] [num] [num] -> doesn't
  (define monitor
    (opt-lambda (email-address
                 server-name
                 [server-port default-server-port]
                 [poll-frequency-seconds default-poll-frequency-seconds]
                 [server-response-timeout-seconds default-server-response-timeout-seconds])
      
      (define result-channel (make-channel))
      
      (define (send-email msg)
        (send-email-alert email-address server-name server-port msg))
      
      (let check-server ()
        (poke-web-server result-channel server-name server-port server-response-timeout-seconds)
        (let ([result (channel-get result-channel)])
          (match result
            [`(ok) (void)]
            [else (send-email (result->message result))])
          (sleep poll-frequency-seconds)
          (check-server))))))