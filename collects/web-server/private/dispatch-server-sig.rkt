#lang racket
(require web-server/private/util
         web-server/private/connection-manager)

(define-signature dispatch-server^
  ((contracted
    [serve (-> (-> void))]
    [serve-ports (input-port? output-port? . -> . (-> void))])))

(define-signature dispatch-server-config^
  ((contracted
    [port port-number?]
    [listen-ip (or/c string? false/c)]
    [max-waiting integer?]
    [initial-connection-timeout integer?]
    [read-request
     (connection? 
      port-number?
      (input-port? . -> . (values string? string?))
      . -> .
      (values any/c boolean?))]
    [dispatch 
     (-> connection? any/c void)])))

(provide
 dispatch-server^ dispatch-server-config^)
