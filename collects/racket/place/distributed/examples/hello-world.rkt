#lang racket/base
(require racket/place/distributed
         racket/place)

(provide main
         hello-world)

(define (hello-world)
  (place ch
    (printf "hello-world received: ~a\n" (place-channel-get ch))
    (define HW "Hello World")
    (place-channel-put ch (format "~a\n" HW))
    (printf "hello-world sent: ~a\n" HW)))


(define (main)
  (define-values (vm pl)
    (spawn-vm-supervise-place-thunk-at/2 "localhost"
                                         #:listen-port 6344
                                         (get-current-module-path)
                                         'hello-world))
  (master-event-loop
    vm
    (after-seconds 2
      (dplace-put pl "Hello")
      (printf "master-event-loop received: ~a\n" (dplace-get pl)))

    (after-seconds 6
      (exit 0))
    ))
