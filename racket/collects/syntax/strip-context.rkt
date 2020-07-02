#lang racket/base
(require racket/struct)

(provide strip-context
         replace-context)

(define (strip-context e)
  (replace-context #f e))

(define (replace-context ctx e)
  (cond
   [(syntax? e)
    (datum->syntax ctx
                   (replace-context ctx (syntax-e e))
                   e
                   e)]
   [(pair? e) (cons (replace-context ctx (car e))
                    (replace-context ctx (cdr e)))]
   [(vector? e) (list->vector
                 (map (lambda (e) (replace-context ctx e))
                      (vector->list e)))]
   [(box? e) (box (replace-context ctx (unbox e)))]
   [(prefab-struct-key e)
    => (lambda (k)
         (apply make-prefab-struct
                k
                (replace-context ctx (struct->list e))))]
   [(hash? e)
    (cond
      [(hash-eq? e)
       (for/hasheq ([(k v) (in-hash e)])
         (values (replace-context ctx k)
                 (replace-context ctx v)))]
      [(hash-eqv? e)
       (for/hasheqv ([(k v) (in-hash e)])
         (values (replace-context ctx k)
                 (replace-context ctx v)))]
      [else
       (for/hash ([(k v) (in-hash e)])
         (values (replace-context ctx k)
                 (replace-context ctx v)))])]
   [else e]))
