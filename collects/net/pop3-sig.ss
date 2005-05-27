
(module pop3-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:pop3^)
  
  (define-signature net:pop3^
    ((struct communicator (sender receiver server port state))
     connect-to-server connect-to-server* disconnect-from-server
     authenticate/plain-text
     get-mailbox-status
     get-message/complete get-message/headers get-message/body
     delete-message
     get-unique-id/single get-unique-id/all
     
     make-desired-header extract-desired-headers

     (struct pop3 ())
     (struct cannot-connect ())
     (struct username-rejected ())
     (struct password-rejected ())
     (struct not-ready-for-transaction (communicator))
     (struct not-given-headers (communicator message))
     (struct illegal-message-number (communicator message))
     (struct cannot-delete-message (communicator message))
     (struct disconnect-not-quiet (communicator))
     (struct malformed-server-response (communicator)))))
