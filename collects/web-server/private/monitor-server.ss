(module monitor-server mzscheme
  (require (lib "etc.ss")
           (lib "contract.ss")
           (lib "match.ss"))
  (require "monitor-poke-web-server.ss"
           "monitor-emailer.ss")
  
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
          (check-server)))))
  
  (provide/contract 
   [monitor ((string? string?) (number? number? number?) . opt-> . void)]
   [default-server-port number?]
   [default-poll-frequency-seconds number?]
   [default-server-response-timeout-seconds number?]))