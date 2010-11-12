#lang racket/base
(require syntax/id-table
         "util.rkt")
(provide new-reftable
         reftable-get-phase
         reftable-add-all!
         reftable-add!)

;; ========

;; A RefTable = hash[Phase => free-id-table[bool]]
;; Phase = nat

#|

For the calculations at the end, we only want to consider identifiers
from the expanded module (ie, syntax-source-module = here.)

That means that instead of a free-id-table, we really want a dict/set
that distinguishes between identifiers imported different ways.  eg,
hash keyed on (nom-name, nom-mod). The reason is that a not-from-here
identifier can block/clobber a from-here identifier if they happen to
refer to the same binding. That messes up the analysis.

Temporary solution: only add from-here identifiers to the reftable.

|#

;; new-reftable : -> RefTable
(define (new-reftable)
  (make-hash))

;; reftable-get-phase : RefTable Phase -> free-id-table[bool]
(define (reftable-get-phase refs phase)
  (hash-ref! refs phase (lambda () (make-free-id-table #:phase phase))))

;; reftable-add-all! : RefTable Phase (listof identifier) -> void
(define (reftable-add-all! refs phase ids)
  (let ([id-table (reftable-get-phase refs phase)])
    (for ([id (in-list ids)]
          #:when (here-mpi? (syntax-source-module id)))
      (free-id-table-set! id-table id #t))))

;; reftable-add! : RefTable Phase identifier -> void
(define (reftable-add! refs phase id)
  (reftable-add-all! refs phase (list id)))
