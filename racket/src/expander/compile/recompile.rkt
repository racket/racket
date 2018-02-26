#lang racket/base
(require "../host/linklet.rkt"
         "../eval/reflect.rkt")

(provide compiled-expression-recompile)

(define (compiled-expression-recompile c)
  (unless (compiled-expression? c)
    (raise-argument-error 'compiled-expression-recompile "compiled-expression?" c))
  (cond
    [(linklet-bundle? c)
     (hash->linklet-bundle
      (for/hasheq ([(k v) (in-hash (linklet-bundle->hash c))])
        (cond
          [(linklet? v) (values k (recompile-linklet v))]
          [else (values k v)])))]
    [(linklet-directory? c)
     (hash->linklet-directory
      (for/hasheq ([(k v) (in-hash (linklet-directory->hash c))])
        (cond
          [(compiled-expression? v)
           (values k (compiled-expression-recompile v))]
          [else
           (values k v)])))]
    [else c]))
