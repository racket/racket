#lang racket/base
(require racket/match
         racket/place/define-remote-server)

(define-named-remote-server
 tuple-server

  (define-state h (make-hash))
  (define-rpc (set k v)
    (hash-set! h k v)
    v)
  (define-rpc (get k)
    (hash-ref h k #f))
  (define-cast (hello)
    (printf "Hello from define-cast\n")(flush-output)))
