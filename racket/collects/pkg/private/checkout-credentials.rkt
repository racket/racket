#lang racket/base

(require net/git-checkout
         racket/list
         "config.rkt")

(provide call-with-git-checkout-credentials)

(define (call-with-git-checkout-credentials thunk)
  (let loop ([credentials-list (cons #f (get-git-checkout-credentials))])
    (define credentials (first credentials-list))
    (with-handlers ([exn:fail:git? (λ (exn)
                                     (if (empty? (rest credentials-list))
                                         (raise exn)
                                         (loop (rest credentials-list))))])
      (define c (make-custodian))
      (parameterize ([current-custodian c]
                     [current-git-username (and credentials (hash-ref credentials 'username))]
                     [current-git-password (and credentials (hash-ref credentials 'password))])
        (dynamic-wind
         void
         thunk
         (lambda ()
           (custodian-shutdown-all c)))))))
