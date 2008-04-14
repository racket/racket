#lang scheme

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (top-interaction . form) ...))

(define-syntax-rule (top-interaction . form)
  (strip-context-and-eval (quote-syntax form)))

(define-namespace-anchor a)

(define namespace (namespace-anchor->empty-namespace a))
(parameterize ([current-namespace namespace])
  (namespace-require 'scheme))

(define (strip-context-and-eval e)
  (parameterize ([current-namespace namespace])
    (eval-syntax (namespace-syntax-introduce
                  (strip-context e)))))

(define (strip-context e)
  (cond
   [(syntax? e)
    (datum->syntax #f
                   (strip-context (syntax-e e))
                   e
                   e)]
   [(pair? e) (cons (strip-context (car e))
                    (strip-context (cdr e)))]
   [(vector? e) (list->vector
                 (map strip-context
                      (vector->list e)))]
   [(box? e) (box (strip-context (unbox e)))]
   [(prefab-struct-key e)
    => (lambda (k)
         (apply make-prefab-struct
                (strip-context (cdr (vector->list (struct->vector e))))))]
   [else e]))


