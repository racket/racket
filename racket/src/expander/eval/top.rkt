#lang racket/base
(require "../common/set.rkt"
         "../common/phase.rkt"
         "../common/performance.rkt"
         "../common/parameter-like.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../compile/module-use.rkt"
         "../compile/reserved-symbol.rkt"
         "../host/linklet.rkt"
         "../compile/serialize.rkt"
         "../compile/instance.rkt"
         "../compile/eager-instance.rkt"
         "../compile/compiled-in-memory.rkt"
         "../compile/multi-top.rkt"
         "../compile/namespace-scope.rkt"
         "../compile/linklet.rkt"
         "../expand/context.rkt"
         "../compile/correlated-linklet.rkt"
         "top-level-instance.rkt"
         "multi-top.rkt"
         "protect.rkt")

;; Run a representation of top-level code as produced by `compile-top`;
;; see "compile/main.rkt", "compile/top.rkt", and "compile/multi-top.rkt"

(provide eval-top
         eval-single-top

         compiled-multiple-top?)

(define (eval-single-top c ns)
  (eval-one-top c ns #:single-expression? #t))

(define (compiled-multiple-top? c)
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (and (linklet-directory? ld)
       (not (hash-ref (linklet-directory->hash ld) #f #f))))

(define (eval-top c ns [eval-compiled eval-top] [as-tail? #t])
  (if (compiled-multiple-top? c)
      (eval-multiple-tops c ns eval-compiled as-tail?)
      (eval-one-top c ns as-tail?)))

(define (eval-multiple-tops c ns eval-compiled as-tail?)
  (define (eval-compiled-parts l)
    (let loop ([l l])
      (cond
       [(null? l) void]
       [(null? (cdr l))
        ;; Tail call:
        (eval-compiled (car l) ns as-tail?)]
       [else
        (eval-compiled (car l) ns #f)
        (loop (cdr l))])))
  
  (cond
   [(compiled-in-memory? c)
    (eval-compiled-parts (compiled-in-memory-pre-compiled-in-memorys c))]
   [(hash-ref (linklet-directory->hash c) 'data #f)
    => (lambda (data-ld)
         (eval-compiled-parts
          (create-compiled-in-memorys-using-shared-data
           (compiled-top->compiled-tops c)
           ;; extract data linklet:
           (hash-ref (linklet-bundle->hash (hash-ref (linklet-directory->hash data-ld) #f)) 0)
           ns)))]
   [else
    ;; No shared data? Strage, but we can carry on, anyway:
    (eval-compiled-parts (compiled-top->compiled-tops c))]))

(define (eval-one-top c ns [as-tail? #t]
                      #:single-expression? [single-expression? #f])
  (performance-region
   ['eval (if single-expression? 'transformer 'top)]
   
   (define ld (if (compiled-in-memory? c)
                  (compiled-in-memory-linklet-directory c)
                  c))
   (define h (linklet-bundle->hash (hash-ref (linklet-directory->hash ld) #f)))
   (define link-instance
     (if (compiled-in-memory? c)
         (link-instance-from-compiled-in-memory c (and (not single-expression?) ns))
         (instantiate-linklet (force-compile-linklet (hash-ref h 'link))
                              (list deserialize-instance
                                    (make-eager-instance-instance
                                     #:namespace ns
                                     #:dest-phase (namespace-phase ns)
                                     #:self (namespace-mpi ns)
                                     #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                                     #:inspector (current-code-inspector))))))

   (define orig-phase (hash-ref h 'original-phase))
   (define max-phase (hash-ref h 'max-phase))
   (define phase-shift (phase- (namespace-phase ns) orig-phase))

   (define extra-inspector (and (compiled-in-memory? c)
                                (compiled-in-memory-compile-time-inspector c)))
   (define phase-to-link-extra-inspectorsss
     (if (compiled-in-memory? c)
         (compiled-in-memory-phase-to-link-extra-inspectorsss c)
         #hasheqv()))
   
   (define phase-to-link-modules
     (if (compiled-in-memory? c)
         (compiled-in-memory-phase-to-link-module-uses c)
         (instance-variable-value link-instance 'phase-to-link-modules)))

   ;; Get last thunk to call in tail position:
   (define thunk
     (for/fold ([prev-thunk void]) ([phase (in-range max-phase (sub1 orig-phase) -1)])
       (prev-thunk #f) ;; call a not-last thunk before proceeding with the next phase
       
       (define module-uses (hash-ref phase-to-link-modules phase null))
       (define-values (import-module-instances import-instances)
         (for/lists (mis is) ([mu (in-list module-uses)])
           (namespace-module-use->module+linklet-instances
            ns mu #:phase-shift (phase- (phase+ phase phase-shift)
                                        (module-use-phase mu)))))

       (define phase-ns (namespace->namespace-at-phase ns (phase+ phase phase-shift)))
       
       (define inst (if single-expression?
                        ;; Instance is ignored, so anything will do:
                        link-instance
                        ;; Instance is used:
                        (make-instance-instance
                         #:namespace phase-ns
                         #:phase-shift phase-shift
                         #:self (namespace-mpi ns)
                         #:inspector (namespace-inspector ns)
                         #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                         #:set-transformer! (lambda (name val)
                                              (namespace-set-transformer! ns
                                                                          (phase+ (sub1 phase) phase-shift)
                                                                          name
                                                                          val)))))

       (define linklet (force-compile-linklet (hash-ref h phase #f)))

       (cond
        [linklet
         (check-require-access linklet #:skip-imports 3
                               module-uses import-module-instances (current-code-inspector)
                               extra-inspector
                               (hash-ref phase-to-link-extra-inspectorsss phase #f))
         (define (instantiate tail?)
           ;; Providing a target instance to `instantiate-linklet` means that we get
           ;; the body's results instead of the instance as a result
           (instantiate-linklet linklet
                                (list* top-level-instance
                                       link-instance
                                       inst
                                       import-instances)
                                ;; Instantiation merges with the namespace's current instance:
                                (namespace->instance ns (phase- (phase+ phase phase-shift)
                                                                (namespace-0-phase ns)))
                                ;; No prompt in tail position:
                                (not tail?)))
         ;; Return `instantiate` as the next thunk
         (cond
          [(zero-phase? phase)
           instantiate]
          [single-expression?
           (lambda (tail?)
             (parameterize ([current-namespace phase-ns])
               (instantiate tail?)))]
          [else
           (define ns-1 (namespace->namespace-at-phase phase-ns (sub1 phase)))
           (lambda (tail?)
             (parameterize ([current-namespace phase-ns])
               (parameterize-like
                #:with ([current-expand-context (make-expand-context ns-1)])
                (instantiate tail?))))])]
        [else void])))
   
   ;; Call last thunk tail position --- maybe, since using a prompt if not `as-tail?`
   (thunk as-tail?)))

(define (link-instance-from-compiled-in-memory cim to-ns)
  ;; If the compilation namespace doesn't match the evaluation
  ;; namespace, then we need to adjust syntax object literals to work
  ;; in the new namespace --- the same shifting that happens otherwise
  ;; through deserialization
  (define orig-syntax-literals (compiled-in-memory-syntax-literals cim))
  (define syntax-literals
    (cond
     [(not to-ns) orig-syntax-literals]
     [(namespace-scopes=? (compiled-in-memory-namespace-scopes cim)
                          (extract-namespace-scopes to-ns))
      orig-syntax-literals]
     [else
      (for/vector #:length (vector-length orig-syntax-literals) ([s (in-vector orig-syntax-literals)])
                  (swap-top-level-scopes s
                                         (compiled-in-memory-namespace-scopes cim)
                                         to-ns))]))
  ;; Create the instance:
  (make-instance 'link #f 'constant
                 mpi-vector-id (compiled-in-memory-mpis cim)
                 syntax-literals-id syntax-literals))
