#lang typed-scheme
  
(require typed/private/utils)

(require-typed-struct communicator ([sender : Number] [receiver : Number] [server : String] [port : Number])
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
  [make-desired-header  (String -> String)] ;;-> Regexp
  [extract-desired-headers ((Listof String) (Listof String) -> (Listof String))]) ;;2nd: Of Regexp
#|
;; requires structure inheritance
(require-typed-struct nntp ()]
(require-typed-struct unexpected-response ([code : Number] [text : String])]
(require-typed-struct bad-status-line ([line : String])]
(require-typed-struct premature-close ([communicator : communicator])]
(require-typed-struct bad-newsgroup-line ([line : String])]
(require-typed-struct non-existent-group ([group : String])]
(require-typed-struct article-not-in-group ([article : Number])]
(require-typed-struct no-group-selected ()]
(require-typed-struct article-not-found ([article : Number])]
(require-typed-struct authentication-rejected ()]
|#
