#lang racket/base
(require "../common/phase.rkt"
         "../common/small-hash.rkt"
         "../common/performance.rkt"
         "../syntax/bulk-binding.rkt"
         "../syntax/module-binding.rkt"
         "../common/module-path.rkt"
         "../compile/module-use.rkt"
         "../expand/root-expand-context.rkt"
         "../host/linklet.rkt"
         "namespace.rkt"
         "provided.rkt"
         "registry.rkt"
         (submod "namespace.rkt" for-module))

(provide make-module-namespace
         raise-unknown-module-error

         namespace->module-instance
         namespace->module-namespace
         namespace-install-module-namespace!
         namespace-record-module-instance-attached!
         module-force-bulk-binding!
         
         namespace->module-linklet-info
         (struct-out module-linklet-info)
         
         make-module
         declare-module!
         module-self
         module-requires
         module-provides
         module-primitive?
         module-is-predefined?
         module-cross-phase-persistent?
         module-no-protected?
         module-inspector
         module-submodule-names
         module-supermodule-name
         module-get-all-variables
         module-access
         module-compute-access!
         
         module-instance-namespace
         module-instance-module

         namespace-module-instantiate!
         namespace-module-visit!
         namespace-module-make-available!
         namespace-primitive-module-visit!
         namespace-visit-available-modules!
         namespace-run-available-modules!

         namespace-module-use->module+linklet-instances)

(module+ for-module-reflect
  (provide (struct-out module)))

;; ----------------------------------------

(struct module (source-name     ; #f, symbol, or complete path
                self            ; module path index used for a self reference
                requires        ; list of (cons phase list-of-module-path-index)
                provides        ; phase-level -> sym -> binding or (provided binding bool bool); see [*] below
                [access #:mutable] ; phase-level -> sym -> 'provided or 'protected; computed on demand from `provides`
                language-info   ; #f or vector
                min-phase-level ; phase-level
                max-phase-level ; phase-level
                phase-level-linklet-info-callback ; phase-level namespace -> module-linklet-info-or-#f
                force-bulk-binding ; bulk-binding-registry -> any
                prepare-instance  ; box namespace phase-shift bulk-binding-registry inspector -> any
                instantiate-phase ; box namespace phase-shift phase-level bulk-binding-registry inspector -> any
                primitive?      ; inline variable values in compiled code?
                is-predefined?  ; always defined on startup?
                cross-phase-persistent?
                no-protected?   ; short cut for checking protected access
                inspector       ; declaration-time inspector
                submodule-names ; associated submodules (i.e, when declared together)
                supermodule-name ; associated supermodule (i.e, when declared together)
                get-all-variables) ; for `module->indirect-exports`
  #:authentic)

;; [*] Beware that tabels in `provides` may map non-interned symbols
;;     to provided bindings, in case something like a lifted
;;     identifier was provided. Since lifting generates a locally
;;     deterministic unreadable symbol that is intended to be specific
;;     to a particular module, `require`ing unreadable symbols can
;;     create collisions. Still, the provided binding is supposed to
;;     be accessible via `dynamic-require`.

(struct module-linklet-info (linklet-or-instance ; #f, linklet, or instance supplied for cross-linking optimization
                             module-uses         ; #f or vector for linklet's imports
                             self                ; self modidx
                             inspector           ; declaration-time inspector
                             extra-inspector     ; optional extra inspector
                             extra-inspectorsss) ; optional extra inspector sets per variable per import
  #:authentic
  #:transparent)

(define (make-module #:source-name [source-name #f]
                     #:self self
                     #:requires [requires null]
                     #:provides provides
                     #:min-phase-level [min-phase-level 0]
                     #:max-phase-level [max-phase-level 0]
                     #:instantiate-phase-callback instantiate-phase
                     #:force-bulk-binding-callback [force-bulk-binding void]
                     #:prepare-instance-callback [prepare-instance void]
                     #:phase-level-linklet-info-callback [phase-level-linklet-info-callback
                                                          (lambda (phase-level ns insp) #f)]
                     #:language-info [language-info #f]
                     #:primitive? [primitive? #f]
                     #:predefined? [predefined? #f]
                     #:cross-phase-persistent? [cross-phase-persistent? primitive?]
                     #:no-protected? [no-protected? #f]
                     #:submodule-names [submodule-names null]
                     #:supermodule-name [supermodule-name #f]
                     #:get-all-variables [get-all-variables (lambda () null)]) ; ok to omit exported
  (module source-name
          self
          (unresolve-requires requires)
          provides
          #f ; access
          language-info
          min-phase-level max-phase-level
          phase-level-linklet-info-callback
          force-bulk-binding
          prepare-instance
          instantiate-phase
          primitive?
          predefined?
          cross-phase-persistent?
          no-protected?
          (current-code-inspector)
          submodule-names
          supermodule-name
          get-all-variables))

(struct module-instance (namespace
                         module                        ; can be #f for the module being expanded
                         [shifted-requires #:mutable]  ; computed on demand; shifted from `module-requires`
                         phase-level-to-state          ; phase-level -> #f, 'available, or 'started
                         [made-available? #:mutable]   ; no #f in `phase-level-to-state`?
                         [attached? #:mutable]         ; whether the instance has been attached elsewhere
                         data-box)                     ; for use by module implementation
  #:authentic)

(define (make-module-instance m-ns m)
  (module-instance m-ns           ; namespace
                   m              ; module
                   #f             ; shifted-requires (not yet computed)
                   (make-small-hasheqv) ; phase-level-to-state
                   #f             ; made-available?
                   #f             ; attached?
                   (box #f)))     ; data-box

;; ----------------------------------------

;; Create a namespace for expanding a module
(define (make-module-namespace ns
                               #:mpi name-mpi
                               #:root-expand-context root-expand-ctx
                               #:for-submodule? for-submodule?)
  (define phase 0) ; always start at 0 when compiling a module
  (define name (module-path-index-resolve name-mpi))
  (define m-ns
    ;; Keeps all module declarations, but makes a fresh space of instances
    (struct-copy namespace (new-namespace ns
                                          #:root-expand-ctx root-expand-ctx
                                          #:register? #f)
                 [mpi name-mpi]
                 [source-name (resolved-module-path-root-name name)]
                 [phase phase]
                 [0-phase phase]
                 [submodule-declarations (if for-submodule?
                                             ;; Same set of submodules:
                                             (namespace-submodule-declarations ns)
                                             ;; Fresh set of submodules:
                                             (make-small-hasheq))]
                 [available-module-instances (make-hasheqv)]
                 [module-instances (make-hasheqv)]
                 [declaration-inspector (current-code-inspector)]))
  (small-hash-set! (namespace-phase-to-namespace m-ns) phase m-ns)
  (define at-phase (make-hasheq))
  (hash-set! (namespace-module-instances m-ns) phase at-phase)
  (hash-set! at-phase name (make-module-instance m-ns #f))
  m-ns)

;; ----------------------------------------

(define (declare-module! ns m mod-name #:with-submodules? [with-submodules? #t])
  (define prior-m (and with-submodules?
                       (hash-ref (module-registry-declarations (namespace-module-registry ns))
                                 mod-name
                                 #f)))
  (define prior-mi (and prior-m
                        (not (eq? m prior-m))
                        (namespace->module-instance ns mod-name (namespace-phase ns))))
  (when (and prior-m (not (eq? m prior-m)))
    (check-redeclaration-ok prior-m prior-mi mod-name))
  (if with-submodules?
      (hash-set! (module-registry-declarations (namespace-module-registry ns)) mod-name m)
      (small-hash-set! (namespace-submodule-declarations ns) mod-name m))
  (when with-submodules?
    ;; Register this module's exports for use in resolving bulk
    ;; bindings, so that bulk bindings can be shared among other
    ;; modules when unmarshaling; we don't do this without
    ;; `with-submodules?` to avoid loeaking submodules being
    ;; expanded, but see also `bind-all-provides!`
    (register-bulk-provide! (namespace-bulk-binding-registry ns)
                            mod-name
                            (module-self m)
                            (module-provides m))
    ;; Tell resolver that the module is declared
    ((current-module-name-resolver) mod-name #f))
  ;; If this module is already instantiated, re-instantiate it
  (when prior-mi
    (define m-ns (module-instance-namespace prior-mi))
    (define states (module-instance-phase-level-to-state prior-mi))
    (define phase (namespace-phase ns))
    (define visit? (eq? 'started (small-hash-ref states (add1 phase) #f)))
    (define run? (eq? 'started (small-hash-ref states phase #f)))

    (define at-phase (hash-ref (namespace-module-instances ns) phase))
    (hash-set! at-phase mod-name (make-module-instance m-ns m))
               
    (when visit?
      (namespace-module-visit! ns (namespace-mpi m-ns) phase))
    (when run?
      (namespace-module-instantiate! ns (namespace-mpi m-ns) phase))))

(define (check-redeclaration-ok prior-m prior-mi mod-name)
  (when (module-cross-phase-persistent? prior-m)
    (raise-arguments-error 'module
                           "cannot redeclare cross-phase persistent module"
                           "module name" mod-name))
  (when (and prior-mi
             (or (module-instance-attached? prior-mi)
                 (not (inspector-superior? (current-code-inspector)
                                           (namespace-inspector (module-instance-namespace prior-mi))))))
    (raise-arguments-error 'module
                           "current code inspector cannot redeclare module"
                           "module name" mod-name)))

(define (raise-unknown-module-error who mod-name)
  (raise-arguments-error who
                         "unknown module" 
                         "module name" mod-name))

(define (namespace->module-linklet-info ns name phase-level)
  (define m (namespace->module ns name))
  (and m
       ((module-phase-level-linklet-info-callback m) phase-level ns (module-inspector m))))

;; ----------------------------------------

(define (namespace->module-instance ns name 0-phase
                                    #:complain-on-failure? [complain-on-failure? #f]
                                    #:check-available-at-phase-level [check-available-at-phase-level #f]
                                    #:unavailable-callback [unavailable-callback void])
  (define mi
    (or (hash-ref (hash-ref (namespace-module-instances ns) 0-phase #hasheq())
                  name
                  #f)
        (let ([c-ns (or (namespace-root-namespace ns) ns)])
          (hash-ref (namespace-module-instances c-ns) name #f))
        (and complain-on-failure?
             (error "no module instance found:" name 0-phase))))
  (if (and mi check-available-at-phase-level)
      (check-availablilty mi check-available-at-phase-level unavailable-callback)
      mi))

(define (namespace-install-module-namespace! ns name 0-phase m existing-m-ns)
  (define m-ns (struct-copy namespace ns
                            [mpi (namespace-mpi existing-m-ns)]
                            [source-name (namespace-source-name existing-m-ns)]
                            [root-expand-ctx (box (unbox (namespace-root-expand-ctx existing-m-ns)))]
                            [phase (namespace-phase existing-m-ns)]
                            [0-phase (namespace-0-phase existing-m-ns)]
                            [phase-to-namespace (make-small-hasheqv)]
                            [phase-level-to-definitions (if (module-cross-phase-persistent? m)
                                                            (namespace-phase-level-to-definitions existing-m-ns)
                                                            (make-small-hasheqv))]
                            [declaration-inspector (module-inspector m)]
                            [inspector (namespace-inspector existing-m-ns)]))
  (define mi (make-module-instance m-ns m))
  (cond
   [(module-cross-phase-persistent? m)
    (small-hash-set! (namespace-phase-to-namespace m-ns) 0 m-ns)
    (small-hash-set! (namespace-phase-level-to-definitions m-ns)
                     0
                     (namespace->definitions existing-m-ns 0))
    (small-hash-set! (namespace-phase-to-namespace m-ns) 1 (namespace->namespace-at-phase m-ns 1))
    (small-hash-set! (namespace-phase-level-to-definitions m-ns)
                     1
                     (namespace->definitions existing-m-ns 1))
    (hash-set! (namespace-module-instances (or (namespace-root-namespace ns) ns))
               name
               mi)
    (small-hash-set! (module-instance-phase-level-to-state mi) 0 'started)]
   [else
    (small-hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
    (small-hash-set! (namespace-phase-level-to-definitions m-ns)
                     0
                     (namespace->definitions existing-m-ns 0))
    (small-hash-set! (module-instance-phase-level-to-state mi) 0 'started)
    (define at-phase (or (hash-ref (namespace-module-instances ns) 0-phase #f)
                         (let ([at-phase (make-hasheq)])
                           (hash-set! (namespace-module-instances ns) 0-phase at-phase)
                           at-phase)))
    (hash-set! at-phase name mi)]))

(define (namespace-create-module-instance! ns name 0-phase m mpi)
  (define m-ns (struct-copy namespace ns
                            [mpi mpi]
                            [source-name (or (module-source-name m)
                                             (resolved-module-path-root-name
                                              (module-path-index-resolve mpi)))]
                            [root-expand-ctx (box #f)] ; maybe set to non-#f by running
                            [phase 0-phase]
                            [0-phase 0-phase]
                            [phase-to-namespace (make-small-hasheqv)]
                            [phase-level-to-definitions (make-small-hasheqv)]
                            [declaration-inspector (module-inspector m)]
                            [inspector (make-inspector (module-inspector m))]))
  (small-hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
  (define mi (make-module-instance m-ns m))
  (if (module-cross-phase-persistent? m)
      (hash-set! (namespace-module-instances ns) name mi)
      (let ([at-phase (or (hash-ref (namespace-module-instances ns) 0-phase #f)
                          (let ([at-phase (make-hasheq)])
                            (hash-set! (namespace-module-instances ns) 0-phase at-phase)
                            at-phase))])
        (hash-set! at-phase name mi)))
  mi)

(define (check-availablilty mi check-available-at-phase-level unavailable-callback)
  (define m (module-instance-module mi))
  (if (and m
           (<= (module-min-phase-level m) (add1 check-available-at-phase-level) (module-max-phase-level m))
           (not (small-hash-ref (module-instance-phase-level-to-state mi) (add1 check-available-at-phase-level) #f)))
      (unavailable-callback mi)
      mi))

(define (namespace->module-namespace ns name 0-phase
                                     #:complain-on-failure? [complain-on-failure? #f]
                                     #:check-available-at-phase-level [check-available-at-phase-level #f]
                                     #:unavailable-callback [unavailable-callback void])
  (define mi (namespace->module-instance ns name 0-phase
                                         #:complain-on-failure? complain-on-failure?
                                         #:check-available-at-phase-level check-available-at-phase-level
                                         #:unavailable-callback unavailable-callback))
  (and mi (module-instance-namespace mi)))

(define (namespace-record-module-instance-attached! ns mod-name phase)
  (define mi (namespace->module-instance ns mod-name phase))
  (set-module-instance-attached?! mi #t))

;; Before attaching amodule declaration to a new namespace, make sure
;; that its syntax deserialization is associated with the original
;; bulk-binding regsitry
(define (module-force-bulk-binding! m ns)
  ((module-force-bulk-binding m) (namespace-bulk-binding-registry ns)))

;; ----------------------------------------

;; Create a module instance as needed, and then run the specified phase;
;; see also `run-module-instance!`, below
(define (namespace-module-instantiate! ns mpi instance-phase #:run-phase [run-phase (namespace-phase ns)]
                                       #:skip-run? [skip-run? #f]
                                       #:otherwise-available? [otherwise-available? #t]
                                       #:seen [seen #hasheq()])
  (unless (module-path-index? mpi)
    (error "not a module path index:" mpi))
  (define name (performance-region
                ['eval 'resolve]
                (module-path-index-resolve mpi #t)))
  (define m (namespace->module ns name))
  (unless m (raise-unknown-module-error 'instantiate name))
  (define (instantiate! instance-phase run-phase ns)
    ;; Get or create a namespace for the module+phase combination:
    (define mi (or (namespace->module-instance ns name instance-phase)
                   (namespace-create-module-instance! ns name instance-phase m mpi)))
    (run-module-instance! mi ns #:run-phase run-phase
                          #:skip-run? skip-run?
                          #:otherwise-available? otherwise-available?
                          #:seen seen))
  ;; If the module is cross-phase persistent, make sure it's instantiated
  ;; at phase 0 and registered in `ns` as phaseless; otherwise
  (cond
   [(module-cross-phase-persistent? m)
    (instantiate! 0 0 (or (namespace-root-namespace ns) ns))]
   [else
    (instantiate! instance-phase run-phase ns)]))

(define (namespace-module-visit! ns mpi instance-phase #:visit-phase [visit-phase (namespace-phase ns)])
  (namespace-module-instantiate! ns mpi instance-phase #:run-phase (add1 visit-phase)))

(define (namespace-module-make-available! ns mpi instance-phase #:visit-phase [visit-phase (namespace-phase ns)])
  (namespace-module-instantiate! ns mpi instance-phase #:run-phase (add1 visit-phase) #:skip-run? #t))

;; The `instance-phase` corresponds to the phase shift for the module
;; instances. The module may have content at different phase levels,
;; which are all consistently shifted. The `run-phase` is an absolute
;; phase that should be immediately run, unless `skip-run?` is true;
;; to put it another way, phase level `(phase- instance-phase
;; run-phase)` within the instance should be run immediately.
;; Normally, the instance is made available at all other non-negative
;; phases, but `#:otherwise-available?` controls that behavior.
(define (run-module-instance! mi ns #:run-phase run-phase
                              #:skip-run? skip-run? 
                              #:otherwise-available? otherwise-available?
                              #:seen [seen #hasheq()])
  (performance-region
   ['eval 'requires]
   ;; Nothing to do if we've run this phase already and made the
   ;; instance sufficiently available:
   (define m-ns (module-instance-namespace mi))
   (define instance-phase (namespace-0-phase m-ns))
   (define run-phase-level (phase- run-phase instance-phase))
   (unless (and (or skip-run?
                    (eq? 'started (small-hash-ref (module-instance-phase-level-to-state mi) run-phase-level #f)))
                (or (not otherwise-available?)
                    (module-instance-made-available? mi)))
     ;; Something to do...
     (define m (module-instance-module mi))
     (unless m
       (error 'require "import cycle detected; trying to run module being expanded"))
     (define mpi (namespace-mpi m-ns))
     (define phase-shift instance-phase) ; instance phase = phase shift
     (define bulk-binding-registry (namespace-bulk-binding-registry m-ns))
     
     (when (hash-ref seen mi #f)
       (error 'require "import cycle detected during module instantiation"))
     
     ;; If we haven't shifted required mpis already, do that
     (unless (module-instance-shifted-requires mi)
       (set-module-instance-shifted-requires!
        mi
        (for/list ([phase+mpis (in-list (module-requires m))])
          (cons (car phase+mpis)
                (for/list ([req-mpi (in-list (cdr phase+mpis))])
                  (module-path-index-shift req-mpi
                                           (module-self m)
                                           mpi))))))

     ;; Recur for required modules:
     (for ([phase+mpis (in-list (module-instance-shifted-requires mi))])
       (define req-phase (car phase+mpis))
       (for ([req-mpi (in-list (cdr phase+mpis))])
         (namespace-module-instantiate! ns req-mpi (phase+ instance-phase req-phase)
                                        #:run-phase run-phase
                                        #:skip-run? skip-run?
                                        #:otherwise-available? otherwise-available?
                                        #:seen (hash-set seen mi #t))))
     
     ;; Run or make available phases of the module body:
     (unless (label-phase? instance-phase)
       (for ([phase-level (in-range (module-max-phase-level m) (sub1 (module-min-phase-level m)) -1)])
         (define phase (phase+ phase-level phase-shift))
         (cond
          [(and (not skip-run?)
                (eqv? phase run-phase))
           ;; This is the phase to make sure that we've run
           (unless (eq? 'started (small-hash-ref (module-instance-phase-level-to-state mi) phase-level #f))
             (small-hash-set! (module-instance-phase-level-to-state mi) phase-level 'started)
             (void (namespace->definitions m-ns phase-level))
             (define p-ns (namespace->namespace-at-phase m-ns phase))
             (define insp (module-inspector m))
             (define data-box (module-instance-data-box mi))
             (define prep (module-prepare-instance m))
             (define go (module-instantiate-phase m))
             (prep data-box p-ns phase-shift mpi bulk-binding-registry insp)
             (go data-box p-ns phase-shift phase-level mpi bulk-binding-registry insp))]
          [(and otherwise-available?
                (not (negative? run-phase))
                (not (small-hash-ref (module-instance-phase-level-to-state mi) phase-level #f)))
           ;; This is a phase to merely make available
           (hash-update! (namespace-available-module-instances ns)
                         phase
                         (lambda (l) (cons mi l))
                         null)
           (small-hash-set! (module-instance-phase-level-to-state mi) phase-level 'available)])))

     (when otherwise-available?
       (set-module-instance-made-available?! mi #t))

     (unless skip-run?
       ;; In case there's no such phase for this module instance, claim 'started
       ;; to short-circuit future attempts:
       (small-hash-set! (module-instance-phase-level-to-state mi) run-phase-level 'started)))))

(define (namespace-visit-available-modules! ns [run-phase (namespace-phase ns)])
  (namespace-run-available-modules! ns (add1 run-phase)))

(define (namespace-run-available-modules! ns [run-phase (namespace-phase ns)])
  (registry-call-with-lock
   (namespace-module-registry ns)
   (lambda ()
     (let loop ()
       (define mis (hash-ref (namespace-available-module-instances ns) run-phase null))
       (unless (null? mis)
         (hash-set! (namespace-available-module-instances ns) run-phase null)
         (for ([mi (in-list (reverse mis))])
           (run-module-instance! mi ns #:run-phase run-phase #:skip-run? #f #:otherwise-available? #f))
         ;; In case instantiation added more reflectively:
         (loop))))))

(define (namespace-primitive-module-visit! ns name)
  (define mi (hash-ref (namespace-module-instances ns) (make-resolved-module-path name)))
  (run-module-instance! mi ns #:run-phase 1 #:skip-run? #f #:otherwise-available? #t))

;; ----------------------------------------

(define (namespace-module-use->module+linklet-instances ns mu 
                                                        #:shift-from [shift-from #f]
                                                        #:shift-to [shift-to #f]
                                                        #:phase-shift phase-shift)
  (define mod (module-use-module mu))
  (define mi
    (namespace->module-instance ns 
                                (module-path-index-resolve
                                 (if shift-from
                                     (module-path-index-shift mod shift-from shift-to)
                                     mod))
                                phase-shift
                                #:complain-on-failure? #t))
  (define m-ns (module-instance-namespace mi))
  (define d (small-hash-ref (namespace-phase-level-to-definitions m-ns) (module-use-phase mu) #f))
  (if d
      (values mi (definitions-variables d))
      (error 'eval (string-append "namespace mismatch: phase level not found;\n"
                                  "  module: ~a\n"
                                  "  phase level: ~a\n"
                                  "  found phase levels: ~a")
             mod
             (module-use-phase mu)
             (small-hash-keys (namespace-phase-level-to-definitions m-ns)))))

;; ----------------------------------------

;; ensure that each module path index is unresolved, so that resolving
;; on instantiation will trigger module loads
(define (unresolve-requires requires)
  (for/list ([phase+mpis (in-list requires)])
    (cons (car phase+mpis)
          (for/list ([req-mpi (in-list (cdr phase+mpis))])
            (module-path-index-unresolve req-mpi)))))

;; ----------------------------------------

(define (module-compute-access! m)
  (define access
    (for/hasheqv ([(phase at-phase) (in-hash (module-provides m))])
      (values phase
              (for/hash ([(sym binding/p) (in-hash at-phase)])
                (values (module-binding-sym (provided-as-binding binding/p))
                        (if (provided-as-protected? binding/p)
                            'protected
                            'provided))))))
  (set-module-access! m access)
  access)
