#lang racket/base
(require racket/place/distributed
         racket/class
         racket/place
         racket/runtime-path
         "bank.rkt"
         "tuple.rkt")
(define-runtime-path bank-path "bank.rkt")
(define-runtime-path tuple-path "tuple.rkt")

(provide main)

(define (main)
  (define remote-node (spawn-remote-racket-node 
                        "localhost" 
                        #:listen-port 6344))
  (define tuple-place (supervise-place-at 
                        remote-node 
                        #:named 'tuple-server 
                        tuple-path 
                        'make-tuple-server))
  (define bank-place  (supervise-place-at 
                        remote-node bank-path 
                        'make-bank))

  (message-router
    remote-node
    (after-seconds 4
      (displayln (bank-new-account bank-place 'user0))
      (displayln (bank-add bank-place 'user0 10))
      (displayln (bank-removeM bank-place 'user0 5)))

    (after-seconds 2
      (define c (connect-to-named-place remote-node 
                                        'tuple-server))
      (define d (connect-to-named-place remote-node 
                                        'tuple-server))
      (tuple-server-hello c)
      (tuple-server-hello d)
      (displayln (tuple-server-set c "user0" 100))
      (displayln (tuple-server-set d "user2" 200))
      (displayln (tuple-server-get c "user0"))
      (displayln (tuple-server-get d "user2"))
      (displayln (tuple-server-get d "user0"))
      (displayln (tuple-server-get c "user2"))
      )
    (after-seconds 8
      (node-send-exit remote-node))
    (after-seconds 10
      (exit 0))))
