#lang racket/base

(require racket/match
         racket/vector
         unstable/struct
         "util.rkt")

(provide replace-modidx)

(define (replace-modidx expr self-modidx)
  (define (inner-update e)
      (match e
        [(app prefab-struct-key (and key (not #f))) 
         (apply make-prefab-struct key 
                (map update
                     (struct->list e)))]
        [(? module-path-index?) 
         (define-values (path mpi) (module-path-index-split e))
         (if (not path)
             self-modidx
             (module-path-index-join path (update mpi)))]
        [(cons a b)
         (cons (update a) (update b))]
        [(? vector?)
         (vector-map update e)]
        [else e]))
  (define-values (first-update update)
    (build-form-memo inner-update))
  (first-update expr))
