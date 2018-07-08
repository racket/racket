#lang racket/base
(require "namespace.rkt"
         "module.rkt"
         "../common/module-path.rkt"
         "../common/phase.rkt"
         "../common/contract.rkt")

(provide namespace-attach-module
         namespace-attach-module-declaration)

(define/who (namespace-attach-module src-namespace
                                     mod-path
                                     [dest-namespace (current-namespace)])
  (do-attach-module who
                    src-namespace mod-path dest-namespace
                    #:attach-instances? #t))

(define/who (namespace-attach-module-declaration src-namespace
                                             mod-path
                                             [dest-namespace (current-namespace)])
  (do-attach-module who
                    src-namespace mod-path dest-namespace
                    #:attach-instances? #f))

(define (do-attach-module who
                          src-namespace mod-path dest-namespace
                          #:attach-instances? [attach-instances? #f])
  (check who namespace? src-namespace)
  (unless (or (module-path? mod-path)
              (resolved-module-path? mod-path))
    (raise-argument-error who "(or/c module-path? resolved-module-path?)" mod-path))
  (check who namespace? dest-namespace)

  (define phase (namespace-phase src-namespace))
  (unless (eqv? phase (namespace-phase dest-namespace))
    (raise-arguments-error who
                           "source and destination namespace phases do not match"
                           "source phase" phase
                           "destination phase" (namespace-phase dest-namespace)))
  
  (define todo (make-hasheq)) ; module name -> phase -> namespace-or-#f
  
  (define missing '#:missing)

  ;; Loop to check and decide what to transfer
  (let loop ([mpi (module-path-index-join
                   (if (resolved-module-path? mod-path)
                       (resolved-module-path->module-path mod-path)
                       mod-path)
                   #f)]
             [phase phase]
             [attach-instances? attach-instances?]
             [attach-phase phase])
    (define mod-name (parameterize ([current-namespace src-namespace])
                       (module-path-index-resolve mpi)))
    (define attach-this-instance? (and attach-instances? (eqv? phase attach-phase)))
    (define m-ns (hash-ref (hash-ref todo mod-name #hasheqv()) phase missing))

    (when (or (eq? missing m-ns)
              (and attach-this-instance? (not m-ns)))
      (define m (namespace->module src-namespace mod-name))
      (unless m
        (raise-arguments-error who
                               "module not declared (in the source namespace)"
                               "module name" mod-name))
      
      (cond
       [(and (module-cross-phase-persistent? m)
             (not (label-phase? phase))
             (not (zero-phase? phase)))
        ;; Always handle a cross-phase persistent module at phase 0, which means
        ;; that all phases will get the same instance if any instance is attached
        (loop mpi 0 attach-instances? 0)]
       [else
        (define already-m (namespace->module dest-namespace mod-name))
        (when (and already-m (not (eq? already-m m)))
          (raise-arguments-error who
                                 "a different declaration is already in the destination namespace"
                                 "module name" mod-name))

        (define-values (m-ns already?)
          (cond
           [(or attach-this-instance?
                (module-cross-phase-persistent? m))
            (define m-ns (namespace->module-namespace src-namespace mod-name phase))
            (unless m-ns
              (raise-arguments-error who
                                     "module not instantiated (in the source namespace)"
                                     "module name" mod-name))
            
            (define already-m-ns (and already-m
                                      (namespace->module-namespace dest-namespace mod-name phase)))
            (when (and already-m-ns
                       (not (eq? m-ns already-m-ns))
                       (not (namespace-same-instance? m-ns already-m-ns)))
              (raise-arguments-error who
                                     "a different instance is already in the destination namespace"
                                     "module name" mod-name))

            (values m-ns (and already-m-ns #t))]
           [else
            (when (and (label-phase? phase)
                       (not (namespace->module-namespace src-namespace mod-name phase)))
              ;; Force instantiation of for-label instance, which ensures that
              ;; required modules are declared
              (parameterize ([current-namespace src-namespace])
                (namespace-module-instantiate! src-namespace mpi phase)))
            
            (values #f (and already-m #t))]))

        (hash-update! todo mod-name (lambda (ht) (hash-set ht phase m-ns)) #hasheqv())

        (unless already?
          (for* ([phase+reqs (in-list (module-requires m))]
                 [req (in-list (cdr phase+reqs))])
            (loop (module-path-index-shift req
                                           (module-self m)
                                           mpi)
                  (phase+ phase (car phase+reqs))
                  attach-instances?
                  attach-phase))
          (for ([submod-name (in-list (module-submodule-names m))])
            (loop (module-path-index-join `(submod "." ,submod-name) mpi)
                  ;; Attach submodules at phase #f, which allows
                  ;; dependencies to be loaded if they're not declared
                  ;; already, since the submodule has not necessarily
                  ;; been instantiated
                  #f
                  #f
                  attach-phase))
          (when (module-supermodule-name m)
            ;; Associated supermodule is treated like an associated submodule
            (loop (module-path-index-join `(submod "..") mpi) #f #f attach-phase)))])))

  ;; Perform decided transfers
  (for* ([(mod-name phases) (in-hash todo)]
         [(phase m-ns) (in-hash phases)])
    (define m (namespace->module src-namespace mod-name))
    (module-force-bulk-binding! m src-namespace)
    (parameterize ([current-namespace dest-namespace])
      (declare-module! dest-namespace m mod-name))
    (when m-ns
      (namespace-record-module-instance-attached! src-namespace mod-name phase)
      (or (namespace->module-namespace dest-namespace mod-name phase)
          (namespace-install-module-namespace! dest-namespace mod-name phase m m-ns))))

  ;; Send resolver notifications for attached declarations
  (define mnr (current-module-name-resolver))
  (parameterize ([current-namespace dest-namespace])
    (for* ([mod-name (in-hash-keys todo)])
      (mnr mod-name src-namespace))))
