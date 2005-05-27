(module monitor-emailer mzscheme
  (require (lib "sendmail.ss" "net")
           (lib "contract.ss"))
  
  (provide/contract [send-email-alert (string? ; email address
                                       string? ; server-name
                                       number? ; server-port 
                                       string? ; message
                                       . -> .
                                       void?)])
  
  ; send-email-alert : send an email to the specified address informing them of the failure.
  (define (send-email-alert alert-address server-name server-port message)
    (send-mail-message alert-address
                       (format "The server ~a:~a is not responding!" server-name server-port)
                       (list alert-address)
                       null
                       null
                       (list message '("" "Fix it ASAP!!!")))))
