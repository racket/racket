#lang scheme

(provide (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (strip-context-and-eval (quote-syntax form)) ...))


(namespace-require 'scheme)

(define (strip-context-and-eval e)
  (eval-syntax (namespace-syntax-introduce
                (strip-context e))))

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


