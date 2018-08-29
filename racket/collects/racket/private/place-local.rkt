#lang racket/base
(require '#%unsafe
         (for-syntax racket/base))

(provide define-place-local)

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
