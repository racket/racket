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
  (let loop ([mpi (module-path-index-join*
                   (if (resolved-module-path? mod-path)
                       (resolved-module-path->module-path mod-path)
                       mod-path)
                   #f)]
             [maybe-mod-name (and (resolved-module-path? mod-path)
                                  mod-path)]
             [phase phase]
             [attach-instances? attach-instances?]
             [attach-phase phase]
             [recur? #t])
    (define mod-name (or maybe-mod-name
                         (parameterize ([current-namespace src-namespace])
                           (module-path-index-resolve mpi))))
    (define m-ns (hash-ref (hash-ref todo mod-name #hasheqv()) phase missing))

    (when (eq? missing m-ns)
      ;; If module instance is available, can use its shifted requires:
      (define mi (namespace->module-instance src-namespace mod-name phase))
      (define m (if mi
                    (module-instance-module mi)
                    (namespace->module src-namespace mod-name)))
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
        (loop mpi mod-name 0 attach-instances? 0 recur?)]
       [else
        (define already-m (namespace->module dest-namespace mod-name))
        (when (and already-m (not (eq? already-m m)))
          (raise-arguments-error who
                                 "a different declaration is already in the destination namespace"
                                 "module name" mod-name))

        (define attach-this-instance? (and attach-instances?
                                           (module-instantiates-at-level? m (phase- attach-phase phase))))

        (define-values (m-ns already?)
          (cond
           [(or attach-this-instance?
                (and (module-cross-phase-persistent? m)
                     (not (label-phase? phase))))
            (define m-ns (namespace->module-namespace src-namespace mod-name phase))
            (unless m-ns
              (raise-arguments-error who
                                     "module not instantiated (in the source namespace)"
                                     "module name" mod-name
                                     "instance phase" phase))
            
            (define already-m-ns (and already-m
                                      (namespace->module-namespace dest-namespace mod-name phase)))
            (when (and already-m-ns
                       (not (eq? m-ns already-m-ns))
                       (not (namespace-same-instance? m-ns already-m-ns (phase- attach-phase phase))))
              (raise-arguments-error who
                                     "a different instance is already in the destination namespace"
                                     "module name" mod-name
                                     "instance phase" phase))

            (values m-ns (and already-m-ns #t))]
           [else
            (when (and (label-phase? phase)
                       (not (namespace->module-namespace src-namespace mod-name phase)))
              ;; Force instantiation of for-label instance, which ensures that
              ;; required modules are declared
              (parameterize ([current-namespace src-namespace])
                (namespace-module-instantiate! src-namespace mpi phase)))

            (values #f (and already-m (not attach-instances?)))]))

        (hash-update! todo mod-name (lambda (ht) (hash-set ht phase m-ns)) #hasheqv())

        (unless (or already? (not recur?))
          (define shifted-requires
            (or (and mi
                     (module-instance-shifted-requires mi))
                (build-module-instance-shifted-requires src-namespace m mpi mod-name)))

          (if (not (module-flattened-requires m))
              ;; per-phase list of mpis
              (for* ([phase+mpis (in-list shifted-requires)]
                     [mpi (in-list (cdr phase+mpis))])
                (when mpi
                  (loop mpi
                        #f
                        (phase+ phase (car phase+mpis))
                        attach-instances?
                        attach-phase
                        #t)))
              ;; per-mpi list of phases
              (for ([mpi+phases (in-list shifted-requires)])
                (define mpi (vector-ref mpi+phases 0))
                (for ([req-phase (in-list (vector-ref mpi+phases 1))])
                  (loop mpi
                        #f
                        (phase+ phase req-phase)
                        attach-instances?
                        attach-phase
                        #f))))

          (when (non-self-derived-module-path-index? mpi)
            (namespace-save-shifted-requires! src-namespace m mod-name shifted-requires))

          (for ([submod-name (in-list (module-submodule-names m))])
            (loop (module-path-index-join* `(submod "." ,submod-name) mpi)
                  (make-resolved-module-path
                   (let ([n (resolved-module-path-name mod-name)])
                     (if (pair? n)
                         (append n (list submod-name))
                         (list n submod-name))))
                  ;; Attach submodules at phase #f, which allows
                  ;; dependencies to be loaded if they're not declared
                  ;; already, since the submodule has not necessarily
                  ;; been instantiated
                  #f
                  #f
                  attach-phase
                  #t))

          (when (module-supermodule-name m)
            ;; Associated supermodule is treated like an associated submodule
            (loop (module-path-index-join* `(submod "..") mpi) #f #f #f attach-phase #t)))])))

  ;; Perform decided transfers
  (for ([(mod-name phases) (in-hash todo)])
    (define m (namespace->module src-namespace mod-name))
    (module-force-bulk-binding! m src-namespace)
    (unless (eq? m (namespace->module dest-namespace mod-name))
      (namespace-copy-shifted-requires! dest-namespace src-namespace mod-name)
      (parameterize ([current-namespace dest-namespace])
        (declare-module! dest-namespace m mod-name)))
    (for ([(m-phase m-ns) (in-hash phases)])
      (when m-ns
        (namespace-record-module-instance-attached! src-namespace mod-name m-phase)
        (or (namespace->module-namespace dest-namespace mod-name m-phase)
            (namespace-install-module-namespace! dest-namespace mod-name m-phase m m-ns phase)))))

  ;; Send resolver notifications for attached declarations
  (define mnr (current-module-name-resolver))
  (parameterize ([current-namespace dest-namespace])
    (for* ([mod-name (in-hash-keys todo)])
      (mnr mod-name src-namespace))))
