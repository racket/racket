#lang typed/racket/base

(require typed/private/utils)

(require-typed-struct/provide path/param ([path : (U String 'up 'same)] [param : (Listof String)]) net/url)

(require-typed-struct/provide
 url ([scheme : (Option String)]
      [user : (Option String)]
      [host : (Option String)]
      [port : (Option Integer)]
      [path-absolute? : Boolean]
      [path : (Listof path/param)]
      [query : (Listof (Pair Symbol (Option String)))]
      [fragment : (Option String)])
 net/url)

(require/opaque-type URL-Exception url-exception? net/url)
(provide URL-Exception url-exception?)

(define-type-alias PortT (case-lambda (url -> Input-Port) (url (Listof String)-> Input-Port)))
(define-type-alias PortT/String (case-lambda (url String -> Input-Port) (url String (Listof String)-> Input-Port)))

(require/typed/provide net/url

  [path->url (Path -> url)]
  [url->path (case-lambda (url -> Path) (url (U 'unix 'windows) -> Path))]

  [file-url-path-convention-type (Parameter (U 'unix 'windows))]

  [get-pure-port PortT]
  [head-pure-port PortT]
  [delete-pure-port PortT]

  [get-impure-port PortT]
  [head-impure-port PortT]
  [delete-impure-port PortT]

  [post-pure-port PortT/String]
  [put-pure-port PortT/String]

  [post-impure-port PortT/String]
  [put-impure-port PortT/String]

  [display-pure-port (Input-Port -> Void)]
  [purify-port (Input-Port -> String)]

  [call/input-url (case-lambda [url url (Input-Port -> Any) -> Any])] ;;FIXME - need polymorphism

  [current-proxy-servers (Parameter (Listof (List String String Integer)))]

  [netscape/string->url (String -> url)]
  [string->url (String -> url)]
  [url->string (url -> String)]
  [combine-url/relative (url String -> url)])

