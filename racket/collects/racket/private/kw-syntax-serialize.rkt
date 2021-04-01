(module kw-syntax-binding "pre-base.rkt"
  (require (prefix-in k: '#%kernel))

  (provide syntax-serialize
           syntax-deserialize)

  (define (syntax-serialize stx
                            #:base-module-path-index [base-mpi #f]
                            #:preserve-property-keys [preserve-prop-keys '()]
                            #:provides-namespace [provides-namespace (current-namespace)])
    (k:syntax-serialize stx base-mpi preserve-prop-keys provides-namespace))

  (define (syntax-deserialize data
                              #:base-module-path-index [base-mpi #f])
    (k:syntax-deserialize data base-mpi)))
