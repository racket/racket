
(module sendmail-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:sendmail^)
  
  (define-signature net:sendmail^
    (send-mail-message/port
     send-mail-message
     (struct no-mail-recipients ()))))

