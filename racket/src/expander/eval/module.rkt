#lang racket/base
(require "../common/promise.rkt"
         "../common/performance.rkt"
         "../common/parameter-like.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/inspector.rkt"
         "../common/phase.rkt"
         "../compile/module-use.rkt"
         "../compile/reserved-symbol.rkt"
         "../common/module-path.rkt"
         "../compile/serialize.rkt"
         "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "../compile/instance.rkt"
         "../compile/compiled-in-memory.rkt"
         "../compile/correlated-linklet.rkt"
         "../expand/context.rkt"
         "../expand/root-expand-context.rkt"
         "root-context.rkt"
         "protect.rkt"
         "module-cache.rkt")

;; Run a representation of top-level code as produced by `compile-module`;
;; see "compile/main.rkt" and "compile/module.rkt"

(provide eval-module
         compiled-module->declaration-instance
         compiled-module->h+declaration-instance
         compiled-module->h
         current-module-declare-as-predefined)

;; Modules that are defined via `embedded-load` can be "predefined",
;; because they can be defined in every place as the embedded load
;; is replayed in each place
(define current-module-declare-as-predefined (make-parameter #f))

(define (eval-module c
                     #:namespace [ns (current-namespace)]
                     #:with-submodules? [with-submodules? #t]
                     #:supermodule-name [supermodule-name #f]) ; for submodules declared with module
  (performance-region
   ['eval 'module]

   (define-values (dh h data-instance declaration-instance)
     (compiled-module->dh+h+data-instance+declaration-instance c))

   (define syntax-literals-data-instance
     (if (compiled-in-memory? c)
         (make-syntax-literal-data-instance-from-compiled-in-memory c)
         (let ([l (hash-ref h 'stx-data #f)])
           (cond
            [l (instantiate-linklet (eval-linklet* l)
                                    (list deserialize-instance
                                          data-instance))]
            [(eq? (hash-ref h 'module->namespace #f) 'empty)
             empty-syntax-literals-instance/empty-namespace]
            [else
             empty-syntax-literals-data-instance]))))
   
   (define (decl key)
     (instance-variable-value declaration-instance key))
   
   (define pre-submodule-names (hash-ref h 'pre null))
   (define post-submodule-names (hash-ref h 'post null))
   (define default-name (hash-ref h 'name 'module))
   
   (define cache-key (make-module-cache-key
                      (and
                       ;; We expect a hash code only for a module
                       ;; loaded independently from its submodules:
                       (null? pre-submodule-names)
                       (null? post-submodule-names)
                       (hash-ref h 'hash-code #f))))

   (define cross-phase-persistent? (hash-ref h 'cross-phase-persistent? #f))
   (define min-phase (hash-ref h 'min-phase 0))
   (define max-phase (hash-ref h 'max-phase 0))
   (define language-info (hash-ref h 'language-info #f))
   
   ;; Evaluate linklets, so that they're JITted just once (on demand).
   ;; Also, filter the bundle hash to just the phase-specific linklets, so that
   ;; we don't retain other info --- especially the syntax-literals linklet.
   (define phases-h (for*/hash ([phase-level (in-range min-phase (add1 max-phase))]
                                [v (in-value (hash-ref h phase-level #f))]
                                #:when v)
                      (values phase-level (eval-linklet* v))))
   (define syntax-literals-linklet (let ([l (hash-ref h 'stx #f)])
                                     (and l (eval-linklet* l))))

   (define extra-inspector (and (compiled-in-memory? c)
                                (compiled-in-memory-compile-time-inspector c)))
   (define phase-to-link-extra-inspectorsss
     (if (compiled-in-memory? c)
         (compiled-in-memory-phase-to-link-extra-inspectorsss c)
         #hasheqv()))

   (define requires (decl 'requires))
   (define provides (decl 'provides))
   (define original-self (decl 'self-mpi))
   (define phase-to-link-modules (decl 'phase-to-link-modules))

   (define create-root-expand-context-from-module ; might be used to create root-expand-context
     (make-create-root-expand-context-from-module requires phases-h))
   
   (define declare-submodules
     ;; If there's no `dh`, then it's important not to retain a reference to
     ;; `c`, which could cause the serialized form of syntax objects to
     ;; be retained after deserialization and reachable from the module cache;
     ;; if it's there's a `dh`, though, then we won't be in the module cache
     (if dh
         ;; Callback to declare submodules:
         (lambda (ns names declare-name pre?)
           (if (compiled-in-memory? c)
               (for ([c (in-list (if pre?
                                     (compiled-in-memory-pre-compiled-in-memorys c)
                                     (compiled-in-memory-post-compiled-in-memorys c)))])
                 (eval-module c #:namespace ns #:supermodule-name declare-name))
               (for ([name (in-list names)])
                 (define sm-cd (hash-ref dh name #f))
                 (unless sm-cd (error "missing submodule declaration:" name))
                 (eval-module sm-cd #:namespace ns #:supermodule-name declare-name))))
         ;; Dummy callback to avoid retaining anything:
         void))

   ;; At this point, we've prepared everything anout the module that we
   ;; can while staying independent of a specific declaration or
   ;; specific instance. If we have a hash key for this module, we can
   ;; stash `declare-this-module` for potential reuse later.
   (define declare-this-module
     (lambda (ns) ; namespace for declaration
       (define m (make-module #:source-name (current-module-declare-source)
                              #:self original-self
                              #:requires requires
                              #:provides provides
                              #:language-info language-info
                              #:min-phase-level min-phase
                              #:max-phase-level max-phase
                              #:cross-phase-persistent? cross-phase-persistent?
                              #:predefined? (current-module-declare-as-predefined)
                              #:submodule-names (append pre-submodule-names post-submodule-names)
                              #:supermodule-name supermodule-name
                              #:get-all-variables (lambda () (get-all-variables phases-h))
                              #:phase-level-linklet-info-callback
                              (lambda (phase-level ns insp)
                                (module-linklet-info (hash-ref phases-h phase-level #f)
                                                     (hash-ref phase-to-link-modules phase-level #f)
                                                     original-self
                                                     insp
                                                     extra-inspector
                                                     (hash-ref phase-to-link-extra-inspectorsss phase-level #f)))
                              #:force-bulk-binding-callback
                              (lambda (bulk-binding-registry)
                                ;; Avoids a leak of some namespace's bulk-binding registry into the
                                ;; deserialized syntax of the module, but module caching can still allow
                                ;; a namespace's bulk-binding registry to get saved by the module's
                                ;; deserialized syntax.
                                (force-syntax-deserialize syntax-literals-data-instance bulk-binding-registry))
                              #:prepare-instance-callback
                              (lambda (data-box ns phase-shift self bulk-binding-registry insp)
                                (unless (unbox data-box)
                                  (init-instance-data! data-box cache-key ns
                                                       syntax-literals-linklet data-instance syntax-literals-data-instance
                                                       phase-shift original-self self bulk-binding-registry insp
                                                       create-root-expand-context-from-module)))
                              #:instantiate-phase-callback
                              (lambda (data-box ns phase-shift phase-level self bulk-binding-registry insp)
                                (performance-region
                                 ['eval 'instantiate]
                                 (define syntax-literals-instance (instance-data-syntax-literals-instance
                                                                   (unbox data-box)))
                                 (define phase-linklet (hash-ref phases-h phase-level #f))
                                 
                                 (when phase-linklet
                                   (define module-uses (hash-ref phase-to-link-modules phase-level))
                                   (define-values (import-module-instances import-instances)
                                     (for/lists (mis is) ([mu (in-list module-uses)])
                                       (namespace-module-use->module+linklet-instances
                                        ns mu
                                        #:shift-from original-self
                                        #:shift-to self
                                        #:phase-shift
                                        (phase+ (phase- phase-level (module-use-phase mu))
                                                phase-shift))))

                                   (check-require-access phase-linklet #:skip-imports 2
                                                         module-uses import-module-instances insp
                                                         extra-inspector
                                                         (hash-ref phase-to-link-extra-inspectorsss phase-level #f))

                                   (define module-body-instance-instance
                                     (make-module-body-instance-instance
                                      #:set-transformer! (cond
                                                          [(zero-phase? phase-level)
                                                           (lambda (name val)
                                                             (error 'define-syntax "should not happen at phase level 0"))]
                                                          [else
                                                           (lambda (name val)
                                                             (namespace-set-transformer! ns (sub1 phase-level) name val))])))

                                   (define (instantiate-body)
                                     (instantiate-linklet phase-linklet
                                                          (list* syntax-literals-instance
                                                                 module-body-instance-instance
                                                                 import-instances)
                                                          (namespace->instance ns phase-level)))

                                   (cond
                                    [(zero-phase? phase-level)
                                     (cond
                                      [(zero-phase? phase-shift)
                                       (instantiate-body)]
                                      [else
                                       ;; Need to set the current namespace so that it has the
                                       ;; right phase
                                       (parameterize ([current-namespace ns])
                                         (instantiate-body))])]
                                    [else
                                     ;; For phase level 1 and up, set the expansion context
                                     ;; to point back to the module's info:
                                     (define ns-1 (namespace->namespace-at-phase ns (phase+ phase-shift (sub1 phase-level))))
                                     (parameterize ([current-namespace ns])
                                       (parameterize-like
                                        #:with ([current-expand-context (delay (make-expand-context ns-1))]
                                                [current-module-code-inspector insp])
                                        (instantiate-body)))]))))))

       (define declare-name (substitute-module-declare-name default-name))
       
       (when with-submodules?
         (declare-submodules ns pre-submodule-names declare-name #t))

       (declare-module! ns
                        m
                        declare-name
                        #:with-submodules? with-submodules?)

       (when with-submodules?
         (declare-submodules ns post-submodule-names declare-name #f))))
   
   ;; ----------------------------------------
   
   ;; If we have a hash code, save the prepare module in the cache
   ;; so it can be found by that hash code:
   (when cache-key
     (module-cache-set! cache-key declare-this-module))
   
   (declare-this-module ns)))

;; ----------------------------------------

;; Value in a declaration's `data-box`:
(struct instance-data (syntax-literals-instance cache-key))

(define (init-instance-data! data-box cache-key ns
                             syntax-literals-linklet data-instance syntax-literals-data-instance
                             phase-shift original-self self bulk-binding-registry insp
                             create-root-expand-context-from-module)
  (when (not (load-on-demand-enabled))
    (force-syntax-deserialize syntax-literals-data-instance bulk-binding-registry))
  
  (define inst
    (make-instance-instance
     #:namespace ns
     #:phase-shift phase-shift
     #:self self 
     #:inspector insp
     #:bulk-binding-registry bulk-binding-registry
     #:set-transformer! (lambda (name val) (error "shouldn't get here for the root-ctx linklet"))))
  
  (define syntax-literals-instance
    (if syntax-literals-linklet
        (instantiate-linklet syntax-literals-linklet
                             (list deserialize-instance
                                   data-instance
                                   syntax-literals-data-instance
                                   inst))
        empty-syntax-literals-instance))

  (set-box! data-box (instance-data syntax-literals-instance cache-key))
  
  (define get-encoded-root-expand-ctx
    (instance-variable-value syntax-literals-instance 'get-encoded-root-expand-ctx))
  
  (cond
   [(eq? get-encoded-root-expand-ctx 'empty)
    ;; A `#:empty-namespace` declaration requested a namespace with no initial bindings
    (namespace-set-root-expand-ctx! ns (delay (shift-to-inside-root-context
                                               (make-root-expand-context #:self-mpi self))))]
   [(procedure? get-encoded-root-expand-ctx)
    ;; Root expand context has been preserved; deserialize it on demand
    (namespace-set-root-expand-ctx! ns (delay (shift-to-inside-root-context
                                               (root-expand-context-decode-for-module
                                                (get-encoded-root-expand-ctx)
                                                self))))]
   [else
    ;; Root expand context has not been preserved, because it can be reconstructed
    ;; from module metadata; do that on demand
    (namespace-set-root-expand-ctx! ns (delay (shift-to-inside-root-context
                                               (create-root-expand-context-from-module
                                                ns phase-shift original-self self))))]))

;; ----------------------------------------

(define (force-syntax-deserialize syntax-literals-data-instance bulk-binding-registry)
  (unless (or (eq? syntax-literals-data-instance empty-syntax-literals-data-instance)
              (eq? syntax-literals-data-instance empty-syntax-literals-instance/empty-namespace))
    ;; Since on-demand loading is disabled, force deserialization
    (let ([deserialize-syntax (instance-variable-value syntax-literals-data-instance deserialize-syntax-id)])
      ;; We need to make sure there's something to deserialize; if it's already done
      ;; `deserialize-syntax` has been set to #f
      (when deserialize-syntax
        (deserialize-syntax bulk-binding-registry)))))

;; ----------------------------------------

;; Returns:
;;
;;   dh - hash from linklet directory to access submodules, or #f if
;;   no submodules
;;
;;   h - hash from the module's linklet bundle
;;
(define (compiled-module->dh+h c)
  (define ld/h (if (compiled-in-memory? c)
                   (compiled-in-memory-linklet-directory c)
                   c))
  (define dh (cond
              [(linklet-directory? ld/h)
               ;; has submodules
               (linklet-directory->hash ld/h)]
              [else
               ;; no submodules
               #f]))
  (define h (linklet-bundle->hash (if dh
                                      (hash-ref dh #f)
                                      ld/h)))
  
  (values dh h))

(define (compiled-module->h c)
  (define-values (dh h)
    (compiled-module->dh+h c))
  h)

;; Additionally returns:
;;
;;  data-instance - provides data, either extracted from
;;  compiled-in-memory or instantiated from the bundle
;;
;;  declaration-instance - provides metadata, extracted from the
;;  bundle and linked with `data-instance`
(define (compiled-module->dh+h+data-instance+declaration-instance c)
  (define-values (dh h) (compiled-module->dh+h c))

  (define data-instance
    (if (compiled-in-memory? c)
        (make-data-instance-from-compiled-in-memory c)
        (instantiate-linklet (eval-linklet* (hash-ref h 'data))
                             (list deserialize-instance))))

  (define declaration-instance
    (if (and (compiled-in-memory? c)
             (compiled-in-memory-original-self c))
        (make-declaration-instance-from-compiled-in-memory c)
        (instantiate-linklet (eval-linklet* (hash-ref h 'decl))
                             (list deserialize-instance
                                   data-instance))))
  
  (values dh h data-instance declaration-instance))

(define (compiled-module->declaration-instance c)
  (define-values (dh h data-instance declaration-instance)
    (compiled-module->dh+h+data-instance+declaration-instance c))
  declaration-instance)

(define (compiled-module->h+declaration-instance c)
  (define-values (dh h data-instance declaration-instance)
    (compiled-module->dh+h+data-instance+declaration-instance c))
  (values h declaration-instance))

;; ----------------------------------------

(define (make-data-instance-from-compiled-in-memory cim)
  (make-instance 'data #f 'constant
                 mpi-vector-id (compiled-in-memory-mpis cim)))

(define (make-declaration-instance-from-compiled-in-memory cim)
  (make-instance 'decl #f 'constant
                 'self-mpi (compiled-in-memory-original-self cim)
                 'requires (compiled-in-memory-requires cim)
                 'provides (compiled-in-memory-provides cim)
                 'phase-to-link-modules (compiled-in-memory-phase-to-link-module-uses cim)))

(define (make-syntax-literal-data-instance-from-compiled-in-memory cim)
  (make-instance 'syntax-literal-data #f #f
                 deserialize-syntax-id void
                 deserialized-syntax-vector-id (compiled-in-memory-syntax-literals cim)))

(define empty-syntax-literals-instance/empty-namespace
  (make-instance 'empty-stx/empty-ns #f 'constant
                 get-syntax-literal!-id (lambda (pos) #f)
                 'get-encoded-root-expand-ctx 'empty))

;; ----------------------------------------

(define (get-all-variables phases-h)
  (for/hash ([(phase linklet) (in-hash phases-h)])
    (values phase
            (linklet-export-variables linklet))))

;; ----------------------------------------

(define (eval-linklet* l)
  (eval-linklet (force-compile-linklet l)))
