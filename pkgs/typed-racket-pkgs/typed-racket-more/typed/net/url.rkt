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

(require/opaque-type HTTP-Connection http-connection? net/url)
(provide HTTP-Connection http-connection?)

(define-type-alias PortT (case-lambda (url -> Input-Port) (url (Listof String)-> Input-Port)))
(define-type-alias PortT/String (case-lambda (url String -> Input-Port) (url String (Listof String)-> Input-Port)))
(define-type-alias PortT/Bytes (case-lambda (url Bytes -> Input-Port) (url Bytes (Listof String)-> Input-Port)))
(provide PortT PortT/String PortT/Bytes)

(require/typed/provide net/url

  [path->url (Path-String -> url)]
  [url->path (case-lambda (url -> Path) (url (U 'unix 'windows) -> Path))]

  [file-url-path-convention-type (Parameter (U 'unix 'windows))]

  [get-pure-port
   (case-> (url [#:redirections Natural] -> Input-Port)
           (url (Listof String) [#:redirections Natural] -> Input-Port))]
  [head-pure-port PortT]
  [delete-pure-port PortT]

  [get-impure-port PortT]
  [head-impure-port PortT]
  [delete-impure-port PortT]

  [post-pure-port PortT/Bytes]
  [put-pure-port PortT/Bytes]

  [post-impure-port PortT/Bytes]
  [put-impure-port PortT/Bytes]

  [display-pure-port (Input-Port -> Void)]
  [purify-port (Input-Port -> String)]

  [get-pure-port/headers
   (case-> (url [#:redirections Natural]
                [#:status? Boolean]
                [#:connection (Option HTTP-Connection)]
                -> (values Input-Port String))
           (url (Listof String)
                [#:redirections Natural]
                [#:status? Boolean]
                [#:connection (Option HTTP-Connection)]
                -> (values Input-Port String)))]

  [make-http-connection (-> HTTP-Connection)]
  [http-connection-close (HTTP-Connection -> Void)]

  [call/input-url (case-lambda [url url (Input-Port -> Any) -> Any])] ;;FIXME - need polymorphism

  [current-proxy-servers (Parameter (Listof (List String String Integer)))]

  [http-sendrecv/url
   (url [#:method (U Bytes String Symbol)]
        [#:headers (Listof (U Bytes String))]
        [#:data (Option (U Bytes String))]
        [#:content-decode (Listof Symbol)]
        -> (values Bytes (Listof Bytes) Input-Port))]

  [netscape/string->url (String -> url)]
  [string->url (String -> url)]
  [url->string (url -> String)]
  [combine-url/relative (url String -> url)])

