#lang typed/racket/base

(require typed/private/utils)

(require-typed-struct/provide communicator
			      ([sender : Number] [receiver : Number] [server : String] [port : Number] [state : Symbol])
                  #:extra-constructor-name make-communicator
			      net/pop3)

(require/typed/provide net/pop3
  [connect-to-server (case-lambda (String -> communicator) (String Number -> communicator))]

  [disconnect-from-server (communicator -> Void)]
  [authenticate/plain-text (String String communicator -> Void)]
  [get-mailbox-status (communicator -> (values Number Number))]
  [get-message/complete  (communicator Number -> (values (Listof String)(Listof String)))]
  [get-message/headers  (communicator Number -> (Listof String))]
  [get-message/body (communicator Number -> (Listof String))]
  [delete-message (communicator Number -> Void)]
  [get-unique-id/single  (communicator Number -> String)]
  [get-unique-id/all (communicator -> (Listof (cons Number String)))]

  [make-desired-header  (String -> String)]
  [extract-desired-headers ((Listof String)(Listof String)-> (Listof String))])


(require-typed-struct/provide (pop3 exn)
  () #:extra-constructor-name make-pop3 net/pop3)
(require-typed-struct/provide (cannot-connect pop3)
  () #:extra-constructor-name make-cannot-connect net/pop3)
(require-typed-struct/provide (username-rejected pop3)
  () #:extra-constructor-name make-username-rejected net/pop3)
(require-typed-struct/provide (password-rejected pop3)
  () #:extra-constructor-name make-password-rejected net/pop3)

(require-typed-struct/provide (not-ready-for-transaction pop3)
  ([communicator : communicator])
  #:extra-constructor-name make-not-ready-for-transaction net/pop3)
(require-typed-struct/provide (not-given-headers pop3)
  ([communicator : communicator] [message : Integer])
  #:extra-constructor-name make-not-given-headers net/pop3)
(require-typed-struct/provide (illegal-message-number pop3)
  ([communicator : communicator] [message : Integer])
  #:extra-constructor-name make-illegal-message-number net/pop3)
(require-typed-struct/provide (cannot-delete-message pop3)
  ([communicator : communicator] [message : Integer])
  #:extra-constructor-name make-cannot-delete-message net/pop3)
(require-typed-struct/provide (disconnect-not-quiet pop3)
  ([communicator : communicator])
  #:extra-constructor-name make-disconnect-not-quiet net/pop3)
(require-typed-struct/provide (malformed-server-response pop3)
  ([communicator : communicator])
  #:extra-constructor-name make-malformed-server-response net/pop3)

