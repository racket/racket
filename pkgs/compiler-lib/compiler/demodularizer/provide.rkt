#lang racket/base
(require racket/phase+space
         "binding.rkt"
         "binding-lookup.rkt"
         "name.rkt")

(provide provides-to-names)

(define (provides-to-names provides
                           names transformer-names
                           one-mods
                           excluded-module-mpis included-module-phases
                           #:keep-syntax? keep-syntax?)
  (for/fold ([ht #hasheqv()]) ([(phase+space binds) (in-hash provides)])
    (define root-phase (phase+space-phase phase+space))
    (hash-set ht
              root-phase
              (append
               (for/list ([bind (in-hash-values binds)]
                          #:unless (and (not keep-syntax?) (binding-syntax? bind))
                          #:do [(define-values (sym path/submod phase) (binding-sym-path/submod-phase bind))
                                (define-values (name at-phase)
                                  (binding-lookup path/submod phase sym
                                                  names transformer-names
                                                  one-mods
                                                  excluded-module-mpis included-module-phases))])
                 name)
               (hash-ref ht root-phase null)))))
