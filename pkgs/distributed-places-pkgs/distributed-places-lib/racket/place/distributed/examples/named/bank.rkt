#lang racket/base
(require racket/match
         racket/place/define-remote-server)

(define-remote-server bank
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
