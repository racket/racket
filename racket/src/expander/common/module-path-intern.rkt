#lang racket/base
(require "module-path.rkt"
         (submod "module-path.rkt" for-intern))

(provide make-module-path-index-intern-table
         intern-module-path-index!)

(struct mpi-intern-table (normal ; name[not #f] -[`equal?`-based]-> base -[`eq?`-based]-> module path index
                          fast)) ; superset, but `eq?`-keyed for fast already-interned checks

(define (make-module-path-index-intern-table)
  (mpi-intern-table (make-hash) (make-hasheq)))

(define (intern-module-path-index! t mpi)
  (or (hash-ref (mpi-intern-table-fast t) mpi #f)
      (let-values ([(name base) (module-path-index-split mpi)])
        (cond
         [(not name)
          (hash-set! (mpi-intern-table-fast t) mpi mpi)
          mpi]
         [else
          (define interned-base (and base
                                     (intern-module-path-index! t base)))
          (define at-name 
            (or (hash-ref (mpi-intern-table-normal t) name #f)
                (let ([at-name (make-hasheq)])
                  (hash-set! (mpi-intern-table-normal t) name at-name)
                  at-name)))
          (define i-mpi
            (or (hash-ref at-name interned-base #f)
                (let ([mpi (if (eq? base interned-base)
                               mpi
                               (struct-copy module-path-index mpi
                                            [base interned-base]))])
                  (hash-set! at-name interned-base mpi)
                  mpi)))
          (hash-set! (mpi-intern-table-fast t) mpi i-mpi)
          i-mpi]))))
