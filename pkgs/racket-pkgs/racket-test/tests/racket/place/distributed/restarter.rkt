#lang racket/base
(require racket/place/distributed
         racket/class
         racket/place
         syntax/location)

(provide wait-place-thunk)
(provide main)

(define (wait-place-thunk)
  (place ch
    (printf "BEGINING SLEEP\n")
    (sleep 1)
    (printf "SLEEP DONE\n")))

(define (main)
  (message-router
    (spawn-node-with-place-at
      "localhost"
      #:listen-port 6345
      (quote-module-name)
      'wait-place-thunk
      #:thunk #t
      #:restart-on-exit (restart-every 5
                                       #:retry 3
                                       #:on-final-fail (lambda ()
                                                         (printf "Failed 3 times exititing\n")
                                                         (exit 1))))))
