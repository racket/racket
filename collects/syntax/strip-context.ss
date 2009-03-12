#lang scheme/base

(provide strip-context)

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
                k
                (strip-context (cdr (vector->list (struct->vector e))))))]
   [else e]))
