#lang racket/base
(require "compiled-in-memory.rkt"
         "serialize.rkt"
         "header.rkt"
         "eager-instance.rkt"
         "reserved-symbol.rkt"
         "../host/linklet.rkt")

(provide build-shared-data-linklet)

;; When multiple top-level forms are compiled separately (e.g., for a
;; `begin` sequence), then each has its own serialization of syntax
;; objects and module path indxes, but we want that information to be
;; shared acrosss forms that are compiled together. So, re-serialize
;; the data here in a way that can be shared across the forms.
;;
;; When a multi-form top-level sequence is evaluated, the shared
;; deserialization is propagated to each individual form by
;; reconstructing a `compiled-in-memory` structure and using the same
;; protocol as when top-level forms are evaluated immediately after
;; compilation. See "../eval/multi-top.rkt" for that part, which is
;; the run-time complement to the encoding here.

(define (build-shared-data-linklet cims ns)
  ;; Gather all mpis:
  (define mpis (make-module-path-index-table))
  (define mpi-trees
    (map-cim-tree cims
                  (lambda (cim)
                    (for/vector ([mpi (in-vector (compiled-in-memory-mpis cim))])
                      (add-module-path-index!/pos mpis mpi)))))

  ;; Gather all syntax literals:
  (define syntax-literals (make-syntax-literals))
  (define syntax-literals-trees
    (map-cim-tree cims
                  (lambda (cim)
                    (add-syntax-literals!
                     syntax-literals
                     (compiled-in-memory-syntax-literals cim)))))

  ;; Gather all phase-to-module-uses tables:
  (define module-uses-tables null)
  (define module-uses-tables-count 0)
  (define phase-to-link-module-uses-trees
    (map-cim-tree cims
                  (lambda (cim)
                    (define pos module-uses-tables-count)
                    (set! module-uses-tables (cons (compiled-in-memory-phase-to-link-module-uses cim)
                                                   module-uses-tables))
                    (set! module-uses-tables-count (add1 pos))
                    pos)))
  
  (define syntax-literals-expr
    (generate-eager-syntax-literals!
     syntax-literals
     mpis
     0
     #f ; self
     ns))
  
  (define phase-to-link-module-uses-expr
    `(vector
      ,@(for/list ([phase-to-link-module-uses (in-list (reverse module-uses-tables))])
          (serialize-phase-to-link-module-uses phase-to-link-module-uses mpis))))
  
  (compile-linklet
   `(linklet
     ;; imports
     (,deserialize-imports
      ,eager-instance-imports)
     ;; exports
     (,mpi-vector-id
      mpi-vector-trees
      phase-to-link-modules-vector
      phase-to-link-modules-trees
      syntax-literals
      syntax-literals-trees)
     (define-values (,mpi-vector-id)
       ,(generate-module-path-index-deserialize mpis))
     (define-values (mpi-vector-trees) ',mpi-trees)
     (define-values (phase-to-link-modules-vector) ,phase-to-link-module-uses-expr)
     (define-values (phase-to-link-modules-trees) ',phase-to-link-module-uses-trees)
     (define-values (syntax-literals) ,syntax-literals-expr)
     (define-values (syntax-literals-trees) ',syntax-literals-trees))))

;; ----------------------------------------

(define (map-cim-tree cims proc)
  (let loop ([cims cims])
    (for/list ([cim (in-list cims)])
      (vector (proc cim)
              (loop (compiled-in-memory-pre-compiled-in-memorys cim))
              (loop (compiled-in-memory-post-compiled-in-memorys cim))))))
