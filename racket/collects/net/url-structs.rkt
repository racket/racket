#lang racket/base
(require racket/contract/base racket/serialize)

(define-serializable-struct url
  (scheme user host port path-absolute? path query fragment)
  #:mutable
  #:transparent)
(define-serializable-struct path/param (path param)
  #:transparent)

(provide/contract
 (struct url ([scheme (or/c false/c string?)]
              [user (or/c false/c string?)]
              [host (or/c false/c string?)]
              [port (or/c false/c number?)]
              [path-absolute? boolean?]
              [path (listof path/param?)]
              [query (listof (cons/c symbol? (or/c string? false/c)))]
              [fragment (or/c false/c string?)]))
 (struct path/param ([path (or/c string? (symbols 'up 'same))]
                     [param (listof string?)])))
