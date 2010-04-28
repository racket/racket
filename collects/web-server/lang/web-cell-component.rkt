#lang racket/base
(require (for-syntax racket/base)
         (for-template racket/base))
(provide define-component)

(define-syntax define-component
  (syntax-rules (define)
    [(_ (include-name id formals embed/url) body ...)
     (define include-name
       (lambda formals
         (let/cc k
           (define (id)
             (k
              (lambda (embed/url)
                body ...)))
           (id))))]))
