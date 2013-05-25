#lang racket/base
(require mzlib/contract
         net/url)
(require web-server/http/response-structs
         web-server/http/request-structs
         "../private/util.rkt")           

; configuration-table = (make-configuration-table nat nat num host-table (listof (cons str host-table)))
(define-struct configuration-table
  (port max-waiting initial-connection-timeout default-host virtual-hosts))

; host-table = (make-host-table (listof str) sym messages timeouts paths)
(define-struct host-table (indices log-format messages timeouts paths))

(define-struct host (indices log-format log-path passwords responders timeouts paths))  

(define-struct responders
  (servlet servlet-loading authentication servlets-refreshed passwords-refreshed file-not-found protocol collect-garbage))

; messages = (make-messages str^6)
(define-struct messages
  (servlet authentication servlets-refreshed passwords-refreshed file-not-found protocol collect-garbage))

; timeouts = (make-timeouts nat^5)
(define-struct timeouts (default-servlet password servlet-connection file-per-byte file-base))

; paths = (make-paths str^6)
(define-struct paths (conf host-base log htdocs servlet mime-types passwords))

(provide/contract
 [struct configuration-table
         ([port port-number?]
          [max-waiting exact-nonnegative-integer?]
          [initial-connection-timeout natural-number/c]
          [default-host host-table?]
          [virtual-hosts (listof (cons/c string? host-table?))])]
 [struct host-table 
         ([indices (listof string?)]
          [log-format symbol?]
          [messages messages?]
          [timeouts timeouts?]
          [paths paths?])]
 [struct host 
         ([indices (listof string?)]
          [log-format symbol?]
          [log-path (or/c false/c path-string?)]
          [passwords (or/c false/c path-string?)]
          [responders responders?]
          [timeouts timeouts?]
          [paths paths?])]
 [struct responders
         ([servlet (url? any/c . -> . response?)]
          [servlet-loading (url? any/c . -> . response?)]
          [authentication (url? header? . -> . response?)]
          [servlets-refreshed (-> response?)]
          [passwords-refreshed (-> response?)]
          [file-not-found (request? . -> . response?)]
          [protocol (url? . -> . response?)]
          [collect-garbage (-> response?)])]
 [struct messages
         ([servlet string?]
          [authentication string?]
          [servlets-refreshed string?]
          [passwords-refreshed string?]
          [file-not-found string?]
          [protocol string?]
          [collect-garbage string?])]
 [struct timeouts 
         ([default-servlet number?]
          [password number?]
          [servlet-connection number?]
          [file-per-byte number?]
          [file-base number?])]
 [struct paths 
         ([conf (or/c false/c path-string?)]
          [host-base (or/c false/c path-string?)]
          [log (or/c false/c path-string?)]
          [htdocs (or/c false/c path-string?)]
          [servlet (or/c false/c path-string?)]
          [mime-types (or/c false/c path-string?)]
          [passwords (or/c false/c path-string?)])])
