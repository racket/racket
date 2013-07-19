#lang scheme
(require syntax/strip-context)

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (top-interaction . (#%top-interaction . form)) ...))

(define-syntax-rule (top-interaction . form)
  (strip-context-and-eval (quote-syntax form)))

;; Make a new namespace to run user code. All evaluation has to start
;; with `module-begin' or `top-interaction', and we wrap such
;; evaluations to swap the namespace in and out.

;; One way in which this differs from Racket is that 
;; `#reader'-loaded modules see a different top-level namespace,
;; though it's the same module registry.

(define-namespace-anchor a)
(define namespace (namespace-anchor->empty-namespace a))
(parameterize ([current-namespace namespace])
  (namespace-require 'scheme))

(define (strip-context-and-eval e)
  (let ([ns (current-namespace)])
    (dynamic-wind
        (lambda ()
          (current-namespace namespace))
        (lambda ()
          (call-with-continuation-prompt
           (lambda ()
             (eval-syntax (namespace-syntax-introduce
                           (strip-context e))))
           (default-continuation-prompt-tag)
           (lambda args
             (apply abort-current-continuation 
                    (default-continuation-prompt-tag)
                    args))))
        (lambda ()
          (set! namespace (current-namespace))
          (current-namespace ns)))))
