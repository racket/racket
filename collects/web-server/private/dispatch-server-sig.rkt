#lang racket/base
(require racket/unit
         racket/contract
         racket/async-channel
         web-server/private/util
         unstable/contract
         web-server/private/connection-manager)

(define-signature dispatch-server^
  ((contracted
    [serve (->* () (#:confirmation-channel (or/c false/c async-channel?)) (-> void))]
    [serve-ports (input-port? output-port? . -> . (-> void))])))

(define-signature dispatch-server-config^
  ((contracted
    [port tcp-listen-port?]
    [listen-ip (or/c string? false/c)]
    [max-waiting integer?]
    [initial-connection-timeout integer?]
    [read-request
     (connection? 
      tcp-listen-port?
      (input-port? . -> . (values string? string?))
      . -> .
      (values any/c boolean?))]
    [dispatch 
     (-> connection? any/c void)])))

(provide
 dispatch-server^ dispatch-server-config^)
