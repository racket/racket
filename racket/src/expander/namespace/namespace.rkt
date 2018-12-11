#lang racket/base
(require "../common/promise.rkt"
         "../common/phase.rkt"
         "../common/small-hash.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/module-path.rkt"
         "../expand/root-expand-context.rkt"
         "../host/linklet.rkt"
         "registry.rkt")

(provide make-namespace
         new-namespace
         namespace?
         current-namespace
         namespace-module-registry
         namespace-phase
         namespace-0-phase
         namespace-root-namespace
         namespace-get-root-expand-ctx
         namespace-set-root-expand-ctx!
         namespace-self-mpi
         namespace->namespace-at-phase
         namespace->module
         namespace-mpi
         namespace-source-name
         namespace-bulk-binding-registry
         
         namespace-set-variable!
         namespace-set-consistent!
         namespace-unset-variable!
         namespace-set-transformer!
         namespace-unset-transformer!
         namespace-get-variable
         namespace-get-transformer
         
         namespace-declaration-inspector
         namespace-inspector
         set-namespace-inspector!
         
         namespace->instance
         namespace-same-instance?)

(module+ for-module
  (provide (struct-out namespace)
           (struct-out module-registry)
           (struct-out definitions)
           namespace->definitions))

(struct namespace (mpi                 ; module path index (that's already resolved); instance-specific for a module
                   source-name         ; #f (top-level) or symbol or complete path; user-facing alternative to the mpi
                   root-expand-ctx     ; delay of box of context for top-level expansion; set by module instantiation
                   phase               ; phase (not phase level!) of this namespace
                   0-phase             ; phase of module instance's phase-level 0
                   phase-to-namespace  ; phase -> namespace for same module  [shared for the same module instance]
                   phase-level-to-definitions ; phase-level -> definitions [shared for the same module instance]
                   module-registry     ; module-registry of (resolved-module-path -> module) [shared among modules]
                   bulk-binding-registry ; (resolved-module-path -> bulk-provide) for resolving bulk bindings on unmarshal
                   submodule-declarations ; resolved-module-path -> module [shared during a module compilation]
                   root-namespace      ; #f or namespace for #lang, #reader, and persistent instances [shared among modules]
                   declaration-inspector ; declaration-time inspector
                   [inspector #:mutable] ; instantiation-time inspector
                   available-module-instances  ; phase -> list of module-instance [shared among modules]
                   module-instances)   ; union resolved-module-path -> module-instance        [shared among modules]
  ;;                                   ;       0-phase -> resolved-module-path -> module-instance
  ;;                                   ; where the first option is for cross phase persistent modules
  #:authentic
  #:property prop:custom-write
  (lambda (ns port mode)
    (write-string "#<namespace" port)
    (define n (namespace-source-name ns))
    (when n
      (fprintf port ":~a" (namespace->name ns)))
    (define 0-phase (namespace-0-phase ns))
    (define phase-level (phase- (namespace-phase ns)
                                0-phase))
    (unless (zero-phase? phase-level)
      (fprintf port ":~s" phase-level))
    (unless (zero-phase? 0-phase)
      (fprintf port "~a~s" (if (positive? 0-phase) "+" "") 0-phase))
    (write-string ">" port)))

(struct definitions (variables      ; linklet instance
                     transformers)  ; sym -> val
  #:authentic)

(define (make-namespace)
  (new-namespace))
                        
(define (new-namespace [share-from-ns #f]
                       #:root-expand-ctx [root-expand-ctx (make-root-expand-context
                                                           #:self-mpi top-level-module-path-index)]
                       #:register? [register? #t])
  (define phase (if share-from-ns
                    (namespace-phase share-from-ns)
                    0))
  (define ns
    (namespace top-level-module-path-index
               #f
               (box root-expand-ctx)
               phase
               phase
               (make-small-hasheqv)    ; phase-to-namespace
               (make-small-hasheqv)    ; phase-level-to-definitions
               (if share-from-ns
                   (namespace-module-registry share-from-ns)
                   (make-module-registry))
               (if share-from-ns
                   (namespace-bulk-binding-registry share-from-ns)
                   (make-bulk-binding-registry))
               (make-small-hasheq)     ; submodule-declarations
               (and share-from-ns
                    (or (namespace-root-namespace share-from-ns)
                        share-from-ns))
               #f ; no declaration-time inspector for a top-level namespace
               (make-inspector (current-code-inspector))
               (if share-from-ns
                   (namespace-available-module-instances share-from-ns)
                   (make-hasheqv))
               (if share-from-ns
                   (namespace-module-instances share-from-ns)
                   (make-hasheqv))))
  (when register?
    (small-hash-set! (namespace-phase-to-namespace ns) phase ns))
  ns)

(define current-namespace (make-parameter (make-namespace)
                                          (lambda (v)
                                            (unless (namespace? v)
                                              (raise-argument-error 'current-namespace
                                                                    "namespace?"
                                                                    v))
                                            v)))

(define (namespace-get-root-expand-ctx ns)
  (force (unbox (namespace-root-expand-ctx ns))))

(define (namespace-set-root-expand-ctx! ns root-ctx)
  (set-box! (namespace-root-expand-ctx ns) root-ctx))

(define (namespace-self-mpi ns)
  (root-expand-context-self-mpi (namespace-get-root-expand-ctx ns)))

(define (namespace->module ns name)
  (or (small-hash-ref (namespace-submodule-declarations ns) name #f)
      (hash-ref (module-registry-declarations (namespace-module-registry ns)) name #f)))

(define (namespace->namespace-at-phase ns phase)
  (or (small-hash-ref (namespace-phase-to-namespace ns) phase #f)
      (let ([p-ns (struct-copy namespace ns
                               [phase phase]
                               [root-namespace (or (namespace-root-namespace ns)
                                                   ns)])])
        (small-hash-set! (namespace-phase-to-namespace ns) phase p-ns)
        p-ns)))

(define (namespace->name ns)
  (define n (namespace-source-name ns))
  (define s
    (cond
     [(not n) 'top-level]
     [(symbol? n) (format "'~s" n)]
     [else (string-append "\"" (path->string n) "\"")]))
  (define r (resolved-module-path-name (module-path-index-resolve (namespace-mpi ns))))
  (if (pair? r)
      (string-append "(submod " s " " (substring (format "~s" (cdr r)) 1))
      s))
  
(define (namespace->definitions ns phase-level)
  (define d (small-hash-ref (namespace-phase-level-to-definitions ns) phase-level #f))
  (or d
      (let ()
        (define p-ns (namespace->namespace-at-phase ns (phase+ (namespace-0-phase ns)
                                                               phase-level)))
        (define d (definitions (make-instance (namespace->name p-ns) p-ns) (make-hasheq)))
        (small-hash-set! (namespace-phase-level-to-definitions ns) phase-level d)
        d)))

(define (namespace-set-variable! ns phase-level name val [as-constant? #f])
  (define d (namespace->definitions ns phase-level))
  (instance-set-variable-value! (definitions-variables d) name val (and as-constant? 'constant)))

(define (namespace-set-consistent! ns phase-level name val)
  (define d (namespace->definitions ns phase-level))
  (instance-set-variable-value! (definitions-variables d) name val 'consistent))

(define (namespace-unset-variable! ns phase-level name)
  (define d (namespace->definitions ns phase-level))
  (instance-unset-variable! (definitions-variables d) name))

(define (namespace-set-transformer! ns phase-level name val)
  (define d (namespace->definitions ns (add1 phase-level)))
  (hash-set! (definitions-transformers d) name val))

(define (namespace-unset-transformer! ns phase-level name)
  (define d (namespace->definitions ns (add1 phase-level)))
  (hash-remove! (definitions-transformers d) name))

(define (namespace-get-variable ns phase-level name fail-k)
  (define d (namespace->definitions ns phase-level))
  (instance-variable-value (definitions-variables d) name fail-k))
  
(define (namespace-get-transformer ns phase-level name fail-k)
  (define d (namespace->definitions ns (add1 phase-level)))
  (hash-ref (definitions-transformers d) name fail-k))

(define (namespace->instance ns phase-shift)
  (definitions-variables (namespace->definitions ns phase-shift)))

(define (namespace-same-instance? a-ns b-ns)
  (eq? (small-hash-ref (namespace-phase-level-to-definitions a-ns)
                       0
                       'no-a)
       (small-hash-ref (namespace-phase-level-to-definitions b-ns)
                       0
                       'no-b)))
