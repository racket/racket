#lang racket/base
(require (only-in '#%unsafe
                  unsafe-root-continuation-prompt-tag))

(provide define-parameter-like
         parameterize-like)

;; A parameter-like function differs from a parameter by not being
;; mutable and not being propagated to a new thread. It's just a
;; continuation mark.

(define root-tag (unsafe-root-continuation-prompt-tag))

(define-syntax-rule (define-parameter-like id val)
  (begin
    (define default-val val)
    (define (id)
      (continuation-mark-set-first #f id default-val root-tag))))

(define-syntax parameterize-like
  (syntax-rules ()
    [(_ #:with () body0 body ...)
     (let () body0 body ...)]
    [(_ #:with ([key0 val0] [key val] ...) body0 body ...)
     (with-continuation-mark key0 val0 (parameterize-like #:with ([key val] ...) body0 body ...))]))
