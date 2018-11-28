#lang racket/base
(require "../common/set.rkt"
         "../common/list-ish.rkt"
         "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../syntax/bulk-binding.rkt"
         "../syntax/mapped-name.rkt"
         "../namespace/namespace.rkt"
         "../namespace/provided.rkt"
         "../common/module-path.rkt"
         "../common/module-path-intern.rkt"
         "env.rkt")

(provide make-requires+provides
         requires+provides-self
         requires+provides-can-cross-phase-persistent?
         
         requires+provides-all-bindings-simple?
         set-requires+provides-all-bindings-simple?!
         
         (struct-out required)
         add-required-module!
         add-defined-or-required-id!
         add-bulk-required-ids!
         add-enclosing-module-defined-and-required!
         remove-required-id!
         check-not-defined
         add-defined-syms!
         defined-sym-kind
         extract-module-requires
         extract-module-definitions
         extract-all-module-requires
         
         requires+provides-reset!
         add-provide!
         
         extract-requires-and-provides
         
         shift-provides-module-path-index)

;; ----------------------------------------

(struct requires+provides (self       ; module-path-index to recognize definitions among requires
                           require-mpis ; intern table
                           require-mpis-in-order ; require-phase -> list of module-path-index
                           requires   ; mpi [interned] -> require-phase -> sym -> list-ish of [bulk-]required
                           provides   ; phase -> sym -> binding or protected
                           phase-to-defined-syms ; phase -> sym -> (or/c 'variable 'transformer)
                           also-required ; sym -> binding
                           [can-cross-phase-persistent? #:mutable]
                           [all-bindings-simple? #:mutable]) ; tracks whether bindings are easily reconstructed
  #:authentic)

;; A `required` represents an identifier required into a module
(struct required (id phase can-be-shadowed? as-transformer?) #:authentic)

;; A `nominal` supports a reverse mapping of bindings to nominal info
(struct nominal (module provide-phase require-phase sym) #:transparent #:authentic)

;; A `bulk-required` can be converted into a `required` given the
;; module path, phase, and symbol that are mapped to it
(struct bulk-required (provides ; extract binding info based on the sym
                       prefix-len ; length of a prefix to remove
                       s        ; combine with the sym to create an identifier
                       provide-phase-level ; phase of `provide` in immediately providing module
                       can-be-shadowed?)  ; shadowed because, e.g., an initial import
  #:authentic)

(define (make-requires+provides self
                                #:copy-requires [copy-r+p #f])
  (requires+provides self
                     ;; require-mpis:
                     (if copy-r+p
                         (requires+provides-require-mpis copy-r+p)
                         (make-module-path-index-intern-table))
                     ;; require-mpis-in-order:
                     (if copy-r+p
                         (hash-copy (requires+provides-require-mpis-in-order copy-r+p))
                         (make-hasheqv))
                     (make-hasheq)  ; requires
                     (make-hasheqv) ; provides
                     (make-hasheqv) ; phase-to-defined-syms
                     (make-hasheq)  ; also-required
                     #t
                     #t))

(define (requires+provides-reset! r+p)
  ;; Don't clear `require-mpis-in-order`, since we want to accumulate
  ;; all previously required modules
  (hash-clear! (requires+provides-requires r+p))
  (hash-clear! (requires+provides-provides r+p))
  (hash-clear! (requires+provides-phase-to-defined-syms r+p))
  (hash-clear! (requires+provides-also-required r+p)))

;; ----------------------------------------

(define (intern-mpi r+p mpi)
  (intern-module-path-index! (requires+provides-require-mpis r+p) mpi))

;; ----------------------------------------

;; Register that a module is required at a given phase shift, and return a
;; locally interned module path index
(define (add-required-module! r+p mod-name phase-shift is-cross-phase-persistent?)
  (define mpi (intern-mpi r+p mod-name))
  (unless (hash-ref (hash-ref (requires+provides-requires r+p) mpi #hasheqv()) phase-shift #f)
    ;; Add to list of requires that are kept in order, so that order
    ;; is preserved on instantiation
    (hash-update! (requires+provides-require-mpis-in-order r+p)
                  phase-shift
                  (lambda (l) (cons mpi l))
                  null)
    ;; Init list of required identifiers:
    (hash-set! (hash-ref! (requires+provides-requires r+p) mpi make-hasheqv)
               phase-shift
               (make-hasheq)))
  (unless is-cross-phase-persistent?
    (set-requires+provides-can-cross-phase-persistent?! r+p #f))
  mpi)

;; Register a specific identifier that is required
(define (add-defined-or-required-id! r+p id phase binding
                                     #:can-be-shadowed? [can-be-shadowed? #f]
                                     #:as-transformer? as-transformer?)
  ;; Register specific required identifier
  (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                (module-binding-nominal-require-phase binding)))
    (error "internal error: binding phase does not match nominal info"))
  (add-defined-or-required-id-at-nominal! r+p id phase
                                          #:nominal-module (module-binding-nominal-module binding)
                                          #:nominal-require-phase (module-binding-nominal-require-phase binding)
                                          #:can-be-shadowed? can-be-shadowed?
                                          #:as-transformer? as-transformer?))
                         

;; The internals of `add-defined-or-required-id!` that consumes just
;; the needed part of the binding
(define (add-defined-or-required-id-at-nominal! r+p id phase
                                                #:nominal-module nominal-module
                                                #:nominal-require-phase nominal-require-phase
                                                #:can-be-shadowed? can-be-shadowed?
                                                #:as-transformer? as-transformer?)
  (define at-mod (hash-ref! (requires+provides-requires r+p)
                            (intern-mpi r+p nominal-module)
                            make-hasheqv))
  (define sym-to-reqds (hash-ref! at-mod nominal-require-phase make-hasheq))
  (define sym (syntax-e id))
  ;; Record that the identifier is required
  (hash-set! sym-to-reqds sym (cons-ish (required id phase can-be-shadowed? as-transformer?)
                                        (hash-ref sym-to-reqds sym null))))

;; Like `add-defined-or-required-id!`, but faster for bindings that
;; all have the same scope, etc.<
;; Return #t if any required id is already defined by a shaodwing definition.
(define (add-bulk-required-ids! r+p s self nominal-module phase-shift provides provide-phase-level
                                #:prefix bulk-prefix
                                #:excepts bulk-excepts
                                #:symbols-accum symbols-accum 
                                #:in orig-s
                                #:can-be-shadowed? can-be-shadowed?
                                #:check-and-remove? check-and-remove?
                                #:accum-update-nominals accum-update-nominals
                                #:who who)
  (define phase (phase+ provide-phase-level phase-shift))
  (define shortcut-table (and check-and-remove?
                              ((hash-count provides) . > . 64)
                              (syntax-mapped-names s phase)))
  (define mpi (intern-mpi r+p nominal-module))
  (define at-mod (hash-ref! (requires+provides-requires r+p) mpi make-hasheqv))
  (define sym-to-reqds (hash-ref! at-mod phase-shift make-hasheq))
  (define prefix-len (if bulk-prefix (string-length (symbol->string bulk-prefix)) 0))
  (define br (bulk-required provides prefix-len s provide-phase-level can-be-shadowed?))
  (for/fold ([any-already-defined? #f]) ([(out-sym binding/p) (in-hash provides)]
                                         #:unless (not (symbol-interned? out-sym)))
    (when symbols-accum (hash-set! symbols-accum out-sym #t))
    (cond
      [(hash-ref bulk-excepts out-sym #f)
       #f]
      [else
       (define sym (cond
                     [(not bulk-prefix) out-sym]
                     [else (string->symbol (string-append (symbol->string bulk-prefix)
                                                          (symbol->string out-sym)))]))
       (define already-defined?
         (cond
           [(and check-and-remove?
                 (or (not shortcut-table)
                     (hash-ref shortcut-table sym #f)))
            (check-not-defined #:check-not-required? #t
                               #:allow-defined? #t
                               r+p (datum->syntax s sym s) phase #:in orig-s
                               #:unless-matches
                               (lambda ()
                                 (provide-binding-to-require-binding binding/p
                                                                     sym
                                                                     #:self self
                                                                     #:mpi mpi
                                                                     #:provide-phase-level provide-phase-level
                                                                     #:phase-shift phase-shift))
                               #:remove-shadowed!? #t
                               #:accum-update-nominals accum-update-nominals
                               #:who who)]
           [else #f]))
       (unless already-defined?
         (hash-set! sym-to-reqds sym (cons-ish br (hash-ref sym-to-reqds sym null))))
       (or any-already-defined? already-defined?)])))

;; Convert a combination of a symbol and `bulk-required` to a
;; `required` on demand
(define (bulk-required->required br nominal-module phase sym)
  (define prefix-len (bulk-required-prefix-len br))
  (define out-sym (if (zero? prefix-len)
                      sym
                      (string->symbol (substring (symbol->string sym) prefix-len))))
  (define binding/p (hash-ref (bulk-required-provides br) out-sym))
  (required (datum->syntax (bulk-required-s br) sym)
            (phase+ phase (bulk-required-provide-phase-level br))
            (bulk-required-can-be-shadowed? br)
            (provided-as-transformer? binding/p)))

(define (normalize-required r mod-name phase sym)
  (if (bulk-required? r)
      (bulk-required->required r mod-name phase sym)
      r))

;; Add bindings of an enclosing module
(define (add-enclosing-module-defined-and-required! r+p
                                                    #:enclosing-requires+provides enclosing-r+p
                                                    enclosing-mod
                                                    phase-shift)
  (set-requires+provides-all-bindings-simple?! r+p #f)
  (for ([(mod-name at-mod) (in-hash (requires+provides-requires enclosing-r+p))])
    (for* ([(phase at-phase) (in-hash at-mod)]
           [(sym reqds) (in-hash at-phase)]
           [reqd/maybe-bulk (in-list-ish reqds)])
      (define reqd (normalize-required reqd/maybe-bulk mod-name phase sym))
      (add-defined-or-required-id-at-nominal! r+p
                                              (syntax-shift-phase-level
                                               (syntax-module-path-index-shift 
                                                (required-id reqd)
                                                (requires+provides-self enclosing-r+p)
                                                enclosing-mod)
                                               phase-shift)
                                              (phase+ (required-phase reqd) phase-shift)
                                              #:nominal-module enclosing-mod
                                              #:nominal-require-phase phase-shift
                                              #:can-be-shadowed? #t
                                              #:as-transformer? (required-as-transformer? reqd)))))

;; Removes a required identifier, in anticipation of it being defined.
;; The `check-not-defined` function below is similar, and it also includes
;; an option to remove shadowed bindings.
(define (remove-required-id! r+p id phase #:unless-matches binding)
  (define b (resolve+shift id phase #:exactly? #t))
  (when b
    (define mpi (intern-mpi r+p (module-binding-nominal-module b)))
    (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
    (when at-mod
      (define nominal-phase (module-binding-nominal-require-phase b))
      (define sym-to-reqds (hash-ref at-mod
                                     nominal-phase
                                     #f))
      (when sym-to-reqds
        (define sym (syntax-e id))
        (define l (hash-ref sym-to-reqds sym null))
        (unless (null? l)
          (unless (same-binding? b binding)
            (hash-set! sym-to-reqds sym (remove-non-matching-requireds l id phase mpi nominal-phase sym))))))))

;; Prune a list of `required`s t remove any with a different binding
(define (remove-non-matching-requireds reqds id phase mpi nominal-phase sym)
  ;; Ok to produce a list-ish instead of a list, but we don't have `for*/list-ish`:
  (for*/list ([r (in-list-ish reqds)]
              [r (in-value (normalize-required r mpi nominal-phase sym))]
              #:unless (and (eqv? phase (required-phase r))
                            (free-identifier=? (required-id r) id phase phase)))
    r))

;; Check whether an identifier has a binding that is from a non-shadowable
;; require; if something is found but it will be replaced, then record that
;; bindings are not simple. Returns a boolean to dincate whether the binding
;; is defined already, since `allow-defined?` allows the result to be #t.
(define (check-not-defined #:check-not-required? [check-not-required? #f]
                           #:allow-defined? [allow-defined? #f]
                           r+p id phase #:in orig-s
                           #:unless-matches [ok-binding/delayed #f] ; binding or (-> binding)
                           #:remove-shadowed!? [remove-shadowed!? #f]
                           #:accum-update-nominals [accum-update-nominals #f]
                           #:who who)
  (define b (resolve+shift id phase #:exactly? #t))
  (cond
   [(not b) #f]
   [(not (module-binding? b))
    (raise-syntax-error #f "identifier out of context" id)]
   [else
    (define defined? (and b (eq? (requires+provides-self r+p)
                                 (module-binding-module b))))
    (cond
      [(and defined?
            ;; In case `#%module-begin` is expanded multiple times, check
            ;; that the definition has been seen this particular expansion
            (not (hash-ref (hash-ref (requires+provides-phase-to-defined-syms r+p)
                                     phase
                                     #hasheq())
                           (module-binding-sym b)
                           #f)))
       ;; Doesn't count as previously defined
       #f]
      [else
       (define define-shadowing-require? (and (not defined?) (not check-not-required?)))
       (define mpi (intern-mpi r+p (module-binding-nominal-module b)))
       (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
       (define ok-binding (and (not define-shadowing-require?)
                               (if (procedure? ok-binding/delayed)
                                   (ok-binding/delayed)
                                   ok-binding/delayed)))
       (define (raise-already-bound defined?)
         (raise-syntax-error who
                             (string-append "identifier already "
                                            (if defined? "defined" "required")
                                            (cond
                                              [(zero-phase? phase) ""]
                                              [(label-phase? phase) " for label"]
                                              [(= 1 phase) " for syntax"]
                                              [else (format " for phase ~a" phase)]))
                             orig-s
                             id))
       (cond
         [(and (not at-mod)
               (not define-shadowing-require?))
          ;; Binding is from an enclosing context; if it's from an
          ;; enclosing module, then we've already marked bindings
          ;; a non-simple --- otherwise, we don't care
          #f]
         [(and ok-binding (same-binding? b ok-binding))
          ;; It's the same binding already, so overall binding hasn't
          ;; become non-simple
          (unless (same-binding-nominals? b ok-binding)
            ;; Need to accumulate nominals
            (define (update!)
              (add-binding!
               #:just-for-nominal? #t
               id
               (module-binding-update ok-binding
                                      #:extra-nominal-bindings
                                      (cons b
                                            (module-binding-extra-nominal-bindings b)))
               phase))
            (cond
              [accum-update-nominals
               ;; We can't reset now, because the caller is preparing for
               ;; a bulk bind. Record that we need to merge nominals.
               (set-box! accum-update-nominals (cons update! (unbox accum-update-nominals)))]
              [else (update!)]))
          defined?]
         [(and defined? allow-defined?)
          ;; A `require` doesn't conflict with a definition, even if we
          ;; saw the definition earlier; but make sure there are not multiple
          ;; `require`s (any one of which would be shadowed by the definition)
          (define also-required (requires+provides-also-required r+p))
          (define prev-b (hash-ref also-required (module-binding-sym b) #f))
          (when (and prev-b (not (same-binding? ok-binding prev-b)))
            (raise-already-bound #f))
          (hash-set! also-required (module-binding-sym b) ok-binding)
          (set-requires+provides-all-bindings-simple?! r+p #f)
          #t]
         [else
          (define nominal-phase (module-binding-nominal-require-phase b))
          (define sym-to-reqds (hash-ref at-mod nominal-phase #hasheq()))
          (define reqds (hash-ref sym-to-reqds (syntax-e id) null))
          (define only-can-can-shadow-require?
            (for/fold ([only-can-can-shadow-require? #t]) ([r (in-list-ish reqds)])
              (cond
                [(if (bulk-required? r)
                     (bulk-required-can-be-shadowed? r)
                     (required-can-be-shadowed? r))
                 ;; Shadowing --- ok, but non-simple
                 (set-requires+provides-all-bindings-simple?! r+p #f)
                 only-can-can-shadow-require?]
                [define-shadowing-require? #f]
                [else (raise-already-bound defined?)])))
          (cond
            [define-shadowing-require?
              ;; Not defined, but defining now (shadowing all requires);
              ;; make sure we indicated that the binding is non-simple
              (set-requires+provides-all-bindings-simple?! r+p #f)
              (unless only-can-can-shadow-require?
                ;; Record the `require` binding, if it's non-shadowable,
                ;; in case we see another `require` for the same identifier
                (hash-set! (requires+provides-also-required r+p) (module-binding-sym b) b))]
            [else
             (when (and remove-shadowed!? (not (null? reqds)))
               ;; Same work as in `remove-required-id!`
               (hash-set! sym-to-reqds (syntax-e id)
                          (remove-non-matching-requireds reqds id phase mpi nominal-phase (syntax-e id))))])
          #f])])]))

(define (add-defined-syms! r+p syms phase #:as-transformer? [as-transformer? #f])
  (define phase-to-defined-syms (requires+provides-phase-to-defined-syms r+p))
  (define defined-syms (hash-ref phase-to-defined-syms phase #hasheq()))
  (define new-defined-syms
    (for/fold ([defined-syms defined-syms]) ([sym (in-list syms)])
      (hash-set defined-syms sym (if as-transformer? 'transformer 'variable))))
  (hash-set! phase-to-defined-syms phase new-defined-syms))

;; Returns 'variable, 'transformer, or #f
(define (defined-sym-kind r+p sym phase)
  (define phase-to-defined-syms (requires+provides-phase-to-defined-syms r+p))
  (define defined-syms (hash-ref phase-to-defined-syms phase #hasheq()))
  (hash-ref defined-syms sym #f))

;; Get all the bindings imported from a given module
(define (extract-module-requires r+p mod-name phase)
  (define mpi (intern-mpi r+p mod-name))
  (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
  (and at-mod
       (for*/list ([(sym reqds) (in-hash (hash-ref at-mod phase #hasheq()))]
                   [reqd (in-list-ish reqds)])
         (normalize-required reqd mpi phase sym))))

;; Get all the definitions
(define (extract-module-definitions r+p)
  (or (extract-module-requires r+p (requires+provides-self r+p) 0)
      null))

;; Like `extract-module-requires`, but merging modules and phases
(define (extract-all-module-requires r+p
                                     mod-name ; or #f for "all"
                                     phase)   ; or 'all for "all"
  (define self (requires+provides-self r+p))
  (define requires (requires+provides-requires r+p))
  (let/ec esc
    (for*/list ([mod-name (in-list (if mod-name
                                       (list (intern-mpi r+p mod-name))
                                       (hash-keys requires)))]
                #:unless (eq? mod-name self)
                [phase-to-requireds (in-value (hash-ref requires mod-name #hasheqv()))]
                [phase (in-list (if (eq? phase 'all)
                                    (hash-keys phase-to-requireds)
                                    (list phase)))]
                [(sym reqds) (in-hash
                              (hash-ref phase-to-requireds phase
                                        ;; failure => not required at that phase
                                        (lambda () (esc #f))))]
                [reqd (in-list-ish reqds)])
      (normalize-required reqd mod-name phase sym))))

;; ----------------------------------------

;; Register that a binding is provided as a given symbol; report an
;; error if the provide is inconsistent with an earlier one
(define (add-provide! r+p sym phase binding immed-binding id orig-s
                      #:as-protected? as-protected?
                      #:as-transformer? as-transformer?)
  (when (and as-protected?
             (not (eq? (module-binding-module immed-binding) (requires+provides-self r+p))))
    (raise-syntax-error #f "cannot protect required identifier in re-provide" sym))
  (hash-update! (requires+provides-provides r+p)
                phase
                (lambda (at-phase)
                  (define b/p (hash-ref at-phase sym #f))
                  (define b (provided-as-binding b/p))
                  (cond
                   [(not b)
                    ;; Record this binding, but first strip away any `free-identifier=?`
                    ;; identifier that remains, which means that it doesn't have a binding.
                    ;; The serializer and deserializer won't be able to handle that, and
                    ;; it's not relevant to further comparisons.
                    (define plain-binding (if (binding-free=id binding)
                                              (module-binding-update binding #:free=id #f)
                                              binding))
                    (hash-set at-phase sym (if (or as-protected? as-transformer?)
                                               (provided plain-binding as-protected? as-transformer?)
                                               plain-binding))]
                   [(same-binding? b binding)
                    at-phase]
                   [else
                    (raise-syntax-error #f
                                        "identifier already provided (as a different binding)"
                                        orig-s id)]))
                #hasheq()))

;; ----------------------------------------

(define (extract-requires-and-provides r+p old-self new-self)
  (define (extract-requires)
    ;; Extract from the in-order record, so that instantiation can use the original order
    (define phase-to-mpis-in-order (requires+provides-require-mpis-in-order r+p))
    (define phases-in-order (sort (hash-keys phase-to-mpis-in-order) phase<?))
    (for/list ([phase (in-list phases-in-order)])
      (cons phase
            (for/list ([mpi (in-list (reverse (hash-ref phase-to-mpis-in-order phase)))]
                       #:unless (eq? mpi old-self))
              (module-path-index-shift mpi old-self new-self)))))
  (define (extract-provides)
    (shift-provides-module-path-index (requires+provides-provides r+p)
                                      old-self
                                      new-self))
  (values (extract-requires) (extract-provides)))

;; ----------------------------------------

(define (shift-requires-module-path-index requires from-mpi to-mpi)
  (cond
   [(eq? from-mpi to-mpi) requires]
   [else
    (for/hash ([(phase mpis) (in-hash requires)])
      (values phase
              (for/list ([mpi (in-list mpis)])
                (module-path-index-shift mpi from-mpi to-mpi))))]))

;; Note: the provides may include non-interned symbols. Those may be
;; accessible via` dynamic-require`, but don't import them.
(define (shift-provides-module-path-index provides from-mpi to-mpi)
  (for/hasheqv ([(phase at-phase) (in-hash provides)])
    (values phase
            (for/hasheq ([(sym binding) (in-hash at-phase)])
              (values sym
                      (cond
                       [(eq? from-mpi to-mpi) binding]
                       [else
                        (let loop ([binding binding])
                          (cond
                           [(provided? binding)
                            (provided (loop (provided-binding binding))
                                      (provided-protected? binding)
                                      (provided-syntax? binding))]
                           [else
                            (binding-module-path-index-shift binding from-mpi to-mpi)]))]))))))
