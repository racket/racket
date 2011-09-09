#lang typed/racket/base

(require typed/private/utils)

(require-typed-struct/provide
 communicator ([sender : Number] [receiver : Number] [server : String] [port : Number])
 #:extra-constructor-name make-communicator
 net/nntp)

(require/typed/provide net/nntp
  [connect-to-server (case-lambda (String -> communicator) (String Number -> communicator))]
  [disconnect-from-server (communicator -> Void)]
  [authenticate-user  (communicator String String -> Void)]
  [open-news-group (communicator String -> (values Number Number Number))]
  [head-of-message  (communicator Number -> (Listof String))]
  [body-of-message (communicator Number -> (Listof String))]
  [newnews-since  (communicator Number -> (Listof String))]
  [generic-message-command (communicator Number -> (Listof String))]
  [make-desired-header  (String -> String)]
  [extract-desired-headers ((Listof String) (Listof String) -> (Listof String))])

(require-typed-struct/provide (nntp exn:fail)
  () #:extra-constructor-name make-nntp net/nntp)
(require-typed-struct/provide (unexpected-response nntp)
  ([code : Number] [text : String]) #:extra-constructor-name make-unexpected-response net/nntp)
(require-typed-struct/provide (bad-status-line nntp)
  ([line : String]) #:extra-constructor-name make-bad-status-line net/nntp)
(require-typed-struct/provide (premature-close nntp)
  ([communicator : communicator]) #:extra-constructor-name make-premature-close net/nntp)
(require-typed-struct/provide (bad-newsgroup-line nntp)
  ([line : String]) #:extra-constructor-name make-bad-newsgroup-line net/nntp)
(require-typed-struct/provide (non-existent-group nntp)
  ([group : String]) #:extra-constructor-name make-non-existent-group net/nntp)
(require-typed-struct/provide (article-not-in-group nntp)
  ([article : Number]) #:extra-constructor-name make-article-not-in-group net/nntp)
(require-typed-struct/provide (no-group-selected nntp)
  () #:extra-constructor-name make-no-group-selected net/nntp)
(require-typed-struct/provide (article-not-found nntp)
  ([article : Number]) #:extra-constructor-name make-article-not-found net/nntp)
(require-typed-struct/provide (authentication-rejected nntp)
  () #:extra-constructor-name make-authentication-rejected net/nntp)

