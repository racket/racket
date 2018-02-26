#lang racket/base
(require racket/contract/base
         "private/modresolve-noctc.rkt")

(define rel-to-path-string/c
  (or/c path-string? (cons/c 'submod (cons/c path-string? (listof symbol?)))))

(define rel-to-path-string/thunk/#f
  (or/c rel-to-path-string/c (-> rel-to-path-string/c) false/c))

(provide/contract
 [resolve-module-path (->* (module-path?)
                           (rel-to-path-string/thunk/#f)
                           (or/c path? symbol? 
                                 (cons/c 'submod (cons/c (or/c path? symbol?) 
                                                         (listof symbol?)))))]
 [resolve-module-path-index (->* ((or/c symbol? module-path-index?))
                                 (rel-to-path-string/thunk/#f)
                                 (or/c path? symbol? 
                                       (cons/c 'submod (cons/c (or/c path? symbol?)
                                                               (listof symbol?)))))])
