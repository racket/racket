#lang racket/base
(require (for-syntax racket/base)
         "pthread.rkt")

(provide define-place-local)

;; Just like the one from `racket/private/place-local`, but using the
;; exports of "pthread.rkt" so we can test in bootstrapping mode.

(define-syntax-rule (define-place-local id v)
  (begin
    (define cell (unsafe-make-place-local v))
    (define-syntax id
      (make-set!-transformer
       (lambda (stx)
         (...
          (syntax-case stx (set!)
            [(set! _ r) #'(unsafe-place-local-set! cell r)]
            [(_ e ...) #'((unsafe-place-local-ref cell) e ...)]
            [_ #'(unsafe-place-local-ref cell)])))))))
