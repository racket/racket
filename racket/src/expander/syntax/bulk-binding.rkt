#lang racket/base
(require "../compile/serialize-property.rkt"
         "binding-table.rkt" ; defines `prop:bulk-binding`
         "binding.rkt"
         "../common/module-path.rkt"
         "../namespace/provided.rkt")

(provide provide-binding-to-require-binding

         make-bulk-binding-registry
         register-bulk-provide!
         registered-bulk-provide?

         bulk-binding
         
         bulk-provides-add-prefix-remove-exceptions
         deserialize-bulk-binding)

;; When a require is something like `(require racket/base)`, then
;; we'd like to import the many bindings from `racket/base` in one
;; fast step, and we'd like to share the information in syntax objects
;; from many different modules that all import `racket/base`. A
;; "bulk binding" implements that fast binding and sharing.

;; The difficult part is restoring sharing when a syntax object is
;; unmarshaled, and also leaving the binding information in the
;; providing moduling instead of the requiring module. Keeping the
;; information with the providing module should be ok, because
;; resolving a chain of module imports should ensure that the relevant
;; module is loaded before a syntax object with a bulk binding is used.
;; Still, we have to communicate information from the loading process
;; down the binding-resolving process.

;; A bulk-binding registry manages that connection. The registry is
;; similar to the module registry, in that it maps a resolved module
;; name to provide information. But it has only the provide
;; information, and not the rest of the module's implementation.

;; ----------------------------------------

;; Helper for both regular imports and bulk bindings, which converts a
;; providing module's view of a binding to a requiring mdoule's view.
(define (provide-binding-to-require-binding binding/p   ; the provided binding
                                            sym         ; the symbolic name of the provide
                                            #:self self ; the providing module's view of itself
                                            #:mpi mpi   ; the requiring module's view
                                            #:provide-phase-level provide-phase-level
                                            #:phase-shift phase-shift)
  (define binding (provided-as-binding binding/p))
  (define from-mod (module-binding-module binding))
  (module-binding-update binding
                         #:module (module-path-index-shift from-mod self mpi)
                         #:nominal-module mpi
                         #:nominal-phase provide-phase-level
                         #:nominal-sym sym
                         #:nominal-require-phase phase-shift
                         #:frame-id #f
                         #:extra-inspector (and (not (provided-as-protected? binding/p)) ; see [*] below
                                                (module-binding-extra-inspector binding))
                         #:extra-nominal-bindings null))

;; [*] If a binding has an extra inspector, it's because the binding
;; was provided as a rename transformer with a module (and the rename
;; transformer doesn't have 'not-free-identifier=?). But if we're
;; protecting the rename-transformer output, then the inspector on the
;; providing module should guard the use of the inspector attached to
;; the binding. For now, we approximate(!) that conditional use by
;; just dropping the extra inspector, which means that the original
;; binding (bounding by te rename transformer) is accessible only if
;; the end user has access to the original binding directly.

;; ----------------------------------------

(struct bulk-binding ([provides #:mutable] ; mutable so table can be found lazily on unmarshal
                      prefix               ; #f or a prefix for the import
                      excepts              ; hash table of excluded symbols (before adding prefix)
                      [self #:mutable]     ; the providing module's self
                      mpi                  ; this binding's view of the providing module
                      provide-phase-level  ; providing module's import phase
                      phase-shift          ; providing module's instantiation phase
                      bulk-binding-registry) ; a registry for finding bulk bindings lazily
  #:property prop:bulk-binding
  (bulk-binding-class
   (lambda (b mpi-shifts)
     (or (bulk-binding-provides b)
         ;; Here's where we find provided bindings for unmarshaled syntax
         (let ([mod-name (module-path-index-resolve
                          (apply-syntax-shifts
                           (bulk-binding-mpi b)
                           mpi-shifts))])
           (unless (bulk-binding-bulk-binding-registry b)
             (error "namespace mismatch: no bulk-binding registry available:"
                    mod-name))
           (define table (bulk-binding-registry-table (bulk-binding-bulk-binding-registry b)))
           (define bulk-provide (hash-ref table mod-name #f))
           (unless bulk-provide
             (error "namespace mismatch: bulk bindings not found in registry for module:"
                    mod-name))
           ;; Reset `provide` and `self` to the discovered information
           (set-bulk-binding-self! b (bulk-provide-self bulk-provide))
           (define provides (hash-ref (bulk-provide-provides bulk-provide)
                                      (bulk-binding-provide-phase-level b)))
           ;; Remove exceptions and add prefix
           (define excepts (bulk-binding-excepts b))
           (define prefix (bulk-binding-prefix b))
           (define adjusted-provides
             (cond
               [(or prefix (positive? (hash-count excepts)))
                (bulk-provides-add-prefix-remove-exceptions provides prefix excepts)]
               [else provides]))
           ;; Record the adjusted `provides` table for quick future access:
           (set-bulk-binding-provides! b adjusted-provides)
           adjusted-provides)))
   (lambda (b binding sym)
     ;; Convert the provided binding to a required binding on
     ;; demand during binding resolution
     (provide-binding-to-require-binding
      binding (if (bulk-binding-prefix b)
                  (string->symbol
                   (substring (symbol->string sym)
                              (string-length (symbol->string (bulk-binding-prefix b)))))
                  sym)
      #:self (bulk-binding-self b)
      #:mpi (bulk-binding-mpi b)
      #:provide-phase-level (bulk-binding-provide-phase-level b)
      #:phase-shift (bulk-binding-phase-shift b))))
  #:property prop:serialize
  ;; Serialization drops the `provides` table and the providing module's `self`
  (lambda (b ser-push! reachable-scopes)
    (ser-push! 'tag '#:bulk-binding)
    (ser-push! (bulk-binding-prefix b))
    (ser-push! (bulk-binding-excepts b))
    (ser-push! (bulk-binding-mpi b))
    (ser-push! (bulk-binding-provide-phase-level b))
    (ser-push! (bulk-binding-phase-shift b))
    (ser-push! 'tag '#:bulk-binding-registry)))

(define (deserialize-bulk-binding prefix excepts mpi provide-phase-level phase-shift bulk-binding-registry)
  (bulk-binding #f prefix excepts #f mpi provide-phase-level phase-shift bulk-binding-registry))

(define (bulk-provides-add-prefix-remove-exceptions provides prefix excepts)
  (for/hash ([(sym val) (in-hash provides)]
             #:unless (hash-ref excepts sym #f)
             ;; Don't `require` non-interned
             #:when (symbol-interned? sym))
    (values (if prefix
                (string->symbol (format "~a~a" prefix sym))
                sym)
            val)))

;; ----------------------------------------

;; A blk binding registry has just the provde part of a module, for
;; use in resolving bulk bindings on unmarshal
(struct bulk-provide (self provides))

;; A bulk-binding-registry object is attached to every syntax object
;; in an instantiated module, so that binding resolution on the
;; module's syntax literals can find tables of provided variables
;; based on module names
(struct bulk-binding-registry (table)) ; resolve-module-name -> bulk-provide

(define (make-bulk-binding-registry)
  (bulk-binding-registry (make-hasheq)))

;; Called when a module is instantiated to register its provides:
(define (register-bulk-provide! bulk-binding-registry mod-name self provides)
  (hash-set! (bulk-binding-registry-table bulk-binding-registry)
             mod-name
             (bulk-provide self provides)))

;; Called when a module is imported to make sure that it's in the
;; registry (as opposed to a temporary module instance during
;; expansion):
(define (registered-bulk-provide? bulk-binding-registry mod-name)
  (and (hash-ref (bulk-binding-registry-table bulk-binding-registry) mod-name #f)
       #t))
