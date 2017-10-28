#lang racket/base
(require net/git-checkout
         racket/list
         "config.rkt")

(provide call-with-git-checkout-credentials)

(define (call-with-git-checkout-credentials t)
  (let loop ([credentials-list (cons #f (get-git-checkout-credentials))])
    (define credentials (first credentials-list))
    (with-handlers ([exn:fail:git?
                     (λ (x)
                       (if (empty? (rest credentials-list))
                         (raise x)
                         (loop (rest credentials-list))))])
      (define c (make-custodian))
      (parameterize ([current-custodian c]
                     [current-git-username
                      (and credentials (hash-ref credentials 'username))]
                     [current-git-password
                      (and credentials (hash-ref credentials 'password))])
        (dynamic-wind
         void
         t
         (lambda ()
           (custodian-shutdown-all c)))))))
