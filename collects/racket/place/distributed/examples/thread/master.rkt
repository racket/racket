#lang racket/base
(require racket/place/distributed
         racket/class
         racket/match
         racket/place
         racket/place/define-remote-server
         racket/runtime-path
         syntax/location)

(define-remote-server
  bank

  (define-state accounts (make-hash))
  (define-rpc (new-account who)
     (match (hash-has-key? accounts who)
       [#t '(already-exists)]
       [else
         (hash-set! accounts who 0)
         (list 'created who)]))
  (define-rpc (removeM who amount)
     (cond
       [(hash-ref accounts who (lambda () #f)) =>
          (lambda (balance)
            (cond [(<= amount balance)
                   (define new-balance (- balance amount))
                   (hash-set! accounts who new-balance)
                   (list 'ok new-balance)]
                  [else
                    (list 'insufficient-funds balance)]))]
       [else
         (list 'invalid-account who)]))
  (define-rpc (add who amount)
    (cond
       [(hash-ref accounts who (lambda () #f)) =>
          (lambda (balance)
            (define new-balance (+ balance amount))
            (hash-set! accounts who new-balance)
            (list 'ok new-balance))]
       [else
         (list 'invalid-account who)])))


(provide main)

(define (main)
  (define remote-node (spawn-remote-racket-node "localhost" #:listen-port 6344))
  (define bank-place  (supervise-thread-at remote-node (quote-module-name) 'make-bank))

  (message-router
    remote-node
    (after-seconds 2
      (displayln (bank-new-account bank-place 'user0))
      (displayln (bank-add bank-place 'user0 10))
      (displayln (bank-removeM bank-place 'user0 5)))

    (after-seconds 6
      (node-send-exit remote-node))
    (after-seconds 8
      (exit 0))))
