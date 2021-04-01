#lang racket/base
(require "../common/contract.rkt"
         "../syntax/syntax.rkt"
         "../compile/serialize.rkt"
         "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "../eval/protect.rkt")

(provide syntax-serialize
         syntax-deserialize)

(struct serialized-syntax (version mpis base-mpi-pos data need-registry?)
  #:prefab)

(define/who (syntax-serialize stx
                              [base-mpi #f]
                              [preserve-prop-keys '()]
                              [provides-namespace (current-namespace)])
  (check who syntax? stx)
  (check who module-path-index? #:or-false base-mpi)
  (check who (lambda (l) (and (list? l) (andmap symbol? l)))
         #:contract "(listof symbol?)"
         preserve-prop-keys)
  (check who namespace? #:or-false provides-namespace)
  (define mpis (make-module-path-index-table))
  (define base-mpi-pos (and base-mpi
                            (add-module-path-index!/pos mpis base-mpi)))
  (define data (generate-deserialize stx
                                     #:mpis mpis
                                     #:as-data? #t
                                     #:preserve-prop-keys (for/hasheq ([k (in-list preserve-prop-keys)])
                                                            (values k #t))
                                     #:keep-provides?
                                     (if provides-namespace
                                         (lambda (modname)
                                           (not (namespace->module provides-namespace modname)))
                                         (lambda (modname) #t))))
  (serialized-syntax (version)
                     (generate-module-path-index-deserialize mpis #:as-data? #t)
                     base-mpi-pos
                     data
                     (and provides-namespace #t)))

(define/who (syntax-deserialize data [base-mpi #f])
  (check who module-path-index? #:or-false base-mpi)
  (unless (serialized-syntax? data)
    (raise-arguments-error who "invalid serialized form" "value" data))
  (unless (equal? (version) (serialized-syntax-version data))
    (raise-arguments-error who
                           "version mismatch"
                           "expected" (version)
                           "found" (serialized-syntax-version data)))
  ;; deserialization is unsafe, so only allow it with the original code inspector:
  (unless (eq? (current-code-inspector) initial-code-inspector)
    (error who "deserialization disallowed by code inspector"))
  (define orig-mpis (deserialize-module-path-index-data (serialized-syntax-mpis data)))
  (define orig-base-mpi (and base-mpi
                             (let ([pos (serialized-syntax-base-mpi-pos data)])
                               (and pos
                                    (vector-ref orig-mpis pos)))))
  (define shifted-mpis
    (if orig-base-mpi
        (for/vector #:length (vector-length orig-mpis) ([mpi (in-vector orig-mpis)])
                    (module-path-index-shift mpi orig-base-mpi base-mpi))
        orig-mpis))
  (define bulk-binding-registry (and (serialized-syntax-need-registry? data)
                                     (namespace-bulk-binding-registry (current-namespace))))
  (deserialize-data shifted-mpis #f bulk-binding-registry (serialized-syntax-data data)))
