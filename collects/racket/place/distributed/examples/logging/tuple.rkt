#lang racket/base
(require racket/match
         racket/place/define-remote-server)

(define-named-remote-server
 tuple-server

  (define-state h (make-hash))
  (define-rpc (set k v)
    (hash-set! h k v)
    (log-to-parent #:severity 'debug (format "~a set to ~a" k v))
    v)
  (define-rpc (get k)
    (hash-ref h k #f)))
