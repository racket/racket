#lang racket/base
(require "one-mod.rkt"
         "at-phase-level.rkt")

(provide binding-lookup)

(define (binding-lookup path/submod phase sym
                        names transformer-names
                        one-mods
                        excluded-module-mpis included-module-phases)
  (define one-m (and (not (symbol? path/submod))
                     (hash-ref one-mods path/submod)))
  (cond
    [(or (not one-m)
         (one-mod-excluded? one-m))
     ;; non-demodulized mode, so external name is unchanged
     (values sym phase)]
    [else
     (define src-int-name (or (hash-ref (hash-ref (one-mod-exports one-m)
                                                  phase
                                                  #hasheq())
                                        sym
                                        #f)
                              ;; not mapped as a linklet export; assume
                              ;; that it's a transformer binding, which
                              ;; doesn't exist at the linklet level, so
                              ;; internal and external names effectively
                              ;; match
                              sym))
     (cond
       [(or (hash-ref names (cons (cons path/submod phase) src-int-name) #f)
            (hash-ref transformer-names (cons (cons path/submod phase) src-int-name) #f))
        => (lambda (new-sym)
             ;; Get a potential phase shift
             (define mpi+phase (hash-ref excluded-module-mpis path/submod #f))
             (define phase-shift (if mpi+phase
                                     (cdr mpi+phase)
                                     (hash-ref included-module-phases path/submod 0)))
             (values new-sym (+ phase phase-shift)))]
       [else
        (raise-arguments-error 'demodularize
                               "did not find new name for binding in syntax"
                               "module path" path/submod
                               "name" sym
                               "phase level" phase)])]))
