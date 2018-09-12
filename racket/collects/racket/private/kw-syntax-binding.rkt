(module kw-syntax-binding "pre-base.rkt"
  (require (prefix-in k: '#%kernel))

  (provide syntax-binding-set-extend)

  (define (syntax-binding-set-extend bs as-sym as-phase mpi
                                     #:source-symbol [sym as-sym]
                                     #:source-phase [phase as-phase]
                                     #:nominal-module [nominal-mpi mpi]
                                     #:nominal-phase [nominal-phase phase]
                                     #:nominal-symbol [nominal-sym sym]
                                     #:nominal-require-phase [nominal-require-phase 0]
                                     #:inspector [insp #f])
    (k:syntax-binding-set-extend bs as-sym as-phase mpi
                                 sym phase
                                 nominal-mpi nominal-phase nominal-sym
                                 nominal-require-phase
                                 insp)))
