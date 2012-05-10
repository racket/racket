#lang racket/base
(require racket/unit
         racket/contract
         web-server/private/util
         web-server/configuration/namespace
         web-server/configuration/configuration-table-structs)

(provide
 web-config^)

(define-signature
  web-config^
  ((contracted
    [max-waiting integer?]
    [virtual-hosts (string? . -> . host?)]
    [initial-connection-timeout integer?]
    [port port-number?]
    [listen-ip (or/c false/c string?)]
    [make-servlet-namespace make-servlet-namespace/c])))
