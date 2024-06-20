#lang racket/base
(require "../common/contract.rkt"
         "../syntax/syntax.rkt"
         "../syntax/binding.rkt"
         "../compile/serialize.rkt"
         "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../eval/protect.rkt"
         "bulk-binding.rkt")

(provide syntax-serialize
         syntax-deserialize)

(struct serialized-syntax (version mpis base-mpi-pos data need-registry?)
  #:prefab)

(define/who (syntax-serialize stx
                              [base-mpi #f]
                              [preserve-prop-keys '()]
                              [provides-namespace (current-namespace)]
                              ;; These extra arguments are accessible only by `%#kernel` directly,
                              ;; and not by `syntax-deserialize` from `racket/base`
                              [as-data? #t]
                              [init-mpis '()]
                              [report-mpi-shifts #f] ; #f or (mpi -> any)
                              [map-mpi (lambda (mpi) mpi)]
                              [map-binding-symbol (lambda (mpi sym phase) (values sym phase))])
  (if as-data?
      (check who syntax? stx)
      (check who vector? stx))
  (check who module-path-index? #:or-false base-mpi)
  (check who (lambda (l) (and (list? l) (andmap symbol? l)))
         #:contract "(listof symbol?)"
         preserve-prop-keys)
  (unless (and (not as-data?) (hash? provides-namespace))
    (check who namespace? #:or-false provides-namespace))
  (unless (or as-data?
              (eq? (current-code-inspector) initial-code-inspector))
    (error who "internal serialization disallowed by code inspector"))
  (define mpis (make-module-path-index-table))
  (for ([init-mpi (in-list init-mpis)])
    (add-module-path-index!/pos mpis init-mpi))
  (define base-mpi-pos (and base-mpi
                            (add-module-path-index!/pos mpis base-mpi)))
  (define data (generate-deserialize stx
                                     #:mpis mpis
                                     #:as-data? as-data?
                                     #:preserve-prop-keys (for/hasheq ([k (in-list preserve-prop-keys)])
                                                            (values k #t))
                                     #:keep-provides?
                                     (if provides-namespace
                                         (lambda (modname)
                                           (if (hash? provides-namespace)
                                               (not (hash-ref provides-namespace modname #f))
                                               (not (namespace->module provides-namespace modname))))
                                         (lambda (modname) #t))
                                     #:map-mpi map-mpi
                                     #:map-binding-symbol map-binding-symbol
                                     #:report-mpi-shifts report-mpi-shifts))
  (if as-data?
      (serialized-syntax (version)
                         (generate-module-path-index-deserialize mpis #:as-data? #t)
                         base-mpi-pos
                         data
                         (and provides-namespace #t))
      (values data (module-path-index-table-mpis mpis))))

(define/who syntax-deserialize
  (case-lambda
    [()
     ;; This internal case is accessible only using `%#kernel` directly,
     ;; and not by `syntax-deserialize` from `racket/base`;
     ;; deserialization is unsafe, so only allow it with the original code inspector:
     (unless (eq? (current-code-inspector) initial-code-inspector)
       (error who "deserialization disallowed by code inspector"))
     (define bulk-binding-registry (make-bulk-binding-registry))
     (let loop ([mod-name (make-resolved-module-path '#%builtin)])
       (define m (namespace->module (current-namespace) mod-name))
       (register-bulk-provide! bulk-binding-registry mod-name (module-self m) (module-provides m))
       (for* ([phase+requires (in-list (module-requires m))]
              [req (in-list (cdr phase+requires))])
         (loop (module-path-index-resolve req))))
     (values deserialize-instance bulk-binding-registry register-bulk-provide!
             syntax-shift-module-path-index)]
    [(data) (syntax-deserialize data #f)]
    [(data base-mpi)
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
     (deserialize-data shifted-mpis #f bulk-binding-registry (serialized-syntax-data data))]))

;; keywordless version of internal function
(define (syntax-shift-module-path-index stx from-mpi to-mpi)
  (syntax-module-path-index-shift stx from-mpi to-mpi))
