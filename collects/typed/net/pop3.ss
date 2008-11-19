#lang typed-scheme

(require typed/private/utils)

(require-typed-struct communicator ([sender : Number] [receiver : Number] [server : String] [port : Number] [state : Symbol])net/pop3)

(require/typed/provide net/pop3
  [connect-to-server ( case-lambda (String -> (Opaque communicator?)) (String Number -> (Opaque communicator?)) )] 

  [disconnect-from-server ( (Opaque communicator?) -> Void )]
  [authenticate/plain-text ( String String (Opaque communicator?) -> Void )]
  [get-mailbox-status ( (Opaque communicator?) -> (values Number Number) )]
  [get-message/complete  ( (Opaque communicator?) Number -> (values (Listof String)(Listof String)) )]
  [get-message/headers  ( (Opaque communicator?) Number -> (Listof String) )]
  [get-message/body ( (Opaque communicator?) Number -> (Listof String) )]
  [delete-message ( (Opaque communicator?) Number -> Void )]
  [get-unique-id/single  ( (Opaque communicator?) Number -> String )]
  [get-unique-id/all ( (Opaque communicator?) -> (Listof (cons Number String)) )]
  
  [make-desired-header  ( String -> String )];-> Regexp
  [extract-desired-headers ( (Listof String)(Listof String)-> (Listof String) )];2nd:of Regexp
  )
(provide (struct-out communicator))

#|
(require-typed-struct pop3 ()]
(require-typed-struct cannot-connect ()]
(require-typed-struct username-rejected ()]
(require-typed-struct password-rejected ()]
(require-typed-struct not-ready-for-transaction ([ communicator : (Opaque communicator?) ])net/pop3) 
(require-typed-struct not-given-headers ([ communicator : (Opaque communicator?) ] [message : String])]
(require-typed-struct illegal-message-number ([communicator : (Opaque communicator?)] [message : String])]
(require-typed-struct cannot-delete-message ([communicator : (Opaque communicator?)] [message : String])] 
(require-typed-struct disconnect-not-quiet ([communicator : (Opaque communicator?)])]
(require-typed-struct malformed-server-response ([communicator : (Opaque communicator?)])net/pop3) 
|#

  