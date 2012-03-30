#lang racket/base
(require racket/place/distributed
         racket/place)

(provide hello-world)

(define (hello-world)
  (place ch
    (printf "hello-world received: ~a\n" (place-channel-get ch))
    (define HW "Hello World")
    (place-channel-put ch (format "~a\n" HW))
    (printf "hello-world sent: ~a\n" HW)))


(module+ main
  (define-values (node pl)
    (spawn-node-supervise-place-thunk-at "localhost"
                                         #:listen-port 6344
                                         (quote-module-path "..")
                                         'hello-world))
  (message-router
    node
    (after-seconds 2
      (dplace-put pl "Hello")
      (printf "message-router received: ~a\n" (dplace-get pl)))

    (after-seconds 6 
      (exit 0))))
