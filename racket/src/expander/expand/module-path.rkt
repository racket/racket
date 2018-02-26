#lang racket/base
(require "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "context.rkt")

(provide module-path->mpi
         module-path->mpi/context)

(define (module-path->mpi mod-path self
                          #:declared-submodule-names [declared-submodule-names #hasheq()])
  (cond
   [(and (list? mod-path)
         (= 2 (length mod-path))
         (eq? 'quote (car mod-path))
         (symbol? (cadr mod-path))
         (hash-ref declared-submodule-names (cadr mod-path) #f))
    (module-path-index-join `(submod "." ,(cadr mod-path)) self)]
   [(and (list? mod-path)
         (eq? 'submod (car mod-path))
         (let ([mod-path (cadr mod-path)])
           (and (list? mod-path)
                (= 2 (length mod-path))
                (eq? 'quote (car mod-path))
                (symbol? (cadr mod-path))
                (hash-ref declared-submodule-names (cadr mod-path) #f))))
    (module-path-index-join `(submod "." ,(cadr (cadr mod-path)) ,@(cddr mod-path)) self)]
   [else
    (module-path-index-join mod-path self)]))

(define (module-path->mpi/context mod-path ctx)
  (module-path->mpi mod-path
                    (namespace-mpi (expand-context-namespace ctx))
                    #:declared-submodule-names (expand-context-declared-submodule-names ctx)))
