#lang typed-scheme
  
(require typed/private/utils)

(require-typed-struct/provide
 communicator ([sender : Number] [receiver : Number] [server : String] [port : Number])
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

(require-typed-struct/provide (nntp exn:fail) () net/nntp)
(require-typed-struct/provide (unexpected-response nntp) ([code : Number] [text : String]) net/nntp)
(require-typed-struct/provide (bad-status-line nntp) ([line : String]) net/nntp)
(require-typed-struct/provide (premature-close nntp) ([communicator : communicator]) net/nntp)
(require-typed-struct/provide (bad-newsgroup-line nntp) ([line : String]) net/nntp)
(require-typed-struct/provide (non-existent-group nntp) ([group : String]) net/nntp)
(require-typed-struct/provide (article-not-in-group nntp) ([article : Number]) net/nntp)
(require-typed-struct/provide (no-group-selected nntp) () net/nntp)
(require-typed-struct/provide (article-not-found nntp) ([article : Number]) net/nntp)
(require-typed-struct/provide (authentication-rejected nntp) () net/nntp)

