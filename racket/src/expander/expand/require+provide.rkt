#lang racket/base
(require "../common/set.rkt"
         "../common/list-ish.rkt"
         "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../common/phase.rkt"
         "../common/phase+space.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../syntax/bulk-binding.rkt"
         "../syntax/like-ambiguous-binding.rkt"
         "../syntax/mapped-name.rkt"
         "../syntax/space-scope.rkt"
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

         requires+provides-definitions-shadow-imports?
         disable-definitions-shadow-imports!

         requires+provides-transitive-requires
         
         (struct-out required)
         add-required-space!
         add-required-module!
         add-defined-or-required-id!
         add-bulk-required-ids!
         add-enclosing-module-defined-and-required!
         remove-required-id!
         check-not-defined
         adjust-shadow-requires!
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
                           requires   ; mpi [interned] -> require-phase+space-shift -> sym -> list-ish of [bulk-]required
                           transitive-requires ; resolved-module-path -> pahse-level -> #t ; used to prune instantiates in generate modules
                           provides   ; phase+space -> sym -> binding or protected
                           phase-to-defined-syms ; phase -> sym -> (or/c 'variable 'transformer)
                           also-required ; sym -> binding
                           spaces     ; sym -> #t to track all relevant spaces from requires
                           portal-syntaxes ; phase -> sym -> syntax
                           [can-cross-phase-persistent? #:mutable]
                           [all-bindings-simple? #:mutable] ; tracks whether bindings are easily reconstructed
                           [definitions-shadow-imports? #:mutable])
  #:authentic)

;; A `required` represents an identifier required into a module
(struct required (id phase+space can-be-shadowed? as-transformer?) #:authentic)

;; A `bulk-required` can be converted into a `required` given the
;; module path, phase, and symbol that are mapped to it
(struct bulk-required (provides ; extract binding info based on the sym
                       prefix-len ; length of a prefix to remove
                       s        ; combine with the sym to create an identifier
                       provide-phase+space ; phase+space of `provide` in immediately providing module
                       can-be-shadowed?)  ; shadowed because, e.g., an initial import
  #:authentic)

(define (make-requires+provides self
                                #:copy-requires [copy-r+p #f]
                                #:portal-syntaxes [portal-syntaxes
                                                   (and copy-r+p
                                                        (hash-copy (requires+provides-portal-syntaxes copy-r+p)))])
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
                     (make-hasheq)  ; transitive-requires
                     (make-hasheqv) ; provides
                     (make-hasheqv) ; phase-to-defined-syms
                     (make-hasheq)  ; also-required
                     (make-hasheq)  ; spaces
                     portal-syntaxes
                     #t   ; can-cross-phase-persistent?
                     #t   ; all-bindings-simple?
                     #t)) ; definitions-shadow-imports?

(define (requires+provides-reset! r+p)
  ;; Don't clear `require-mpis-in-order`, since we want to accumulate
  ;; all previously required modules
  (hash-clear! (requires+provides-requires r+p))
  (hash-clear! (requires+provides-transitive-requires r+p)) ; conservative, and may reduce effectiveness
  (hash-clear! (requires+provides-provides r+p))
  (hash-clear! (requires+provides-phase-to-defined-syms r+p))
  (hash-clear! (requires+provides-also-required r+p))
  (hash-clear! (requires+provides-spaces r+p)))

;; ----------------------------------------

(define (intern-mpi r+p mpi)
  (intern-module-path-index! (requires+provides-require-mpis r+p) mpi))

;; ----------------------------------------

(define (add-required-space! r+p space)
  (when space
    (hash-set! (requires+provides-spaces r+p) space #t)))

;; Register that a module is required at a given phase+space level, and return a
;; locally interned module path index
(define (add-required-module! r+p mod-name phase+space-shift is-cross-phase-persistent?)
  (define mpi (intern-mpi r+p mod-name))
  (unless (hash-ref (hash-ref (requires+provides-requires r+p) mpi #hasheqv()) phase+space-shift #f)
    ;; Add to list of requires that are kept in order, so that order
    ;; is preserved on instantiation
    (hash-update! (requires+provides-require-mpis-in-order r+p)
                  (phase+space-shift-phase-level phase+space-shift)
                  (lambda (l) (cons mpi l))
                  null)
    ;; Init list of required identifiers:
    (hash-set! (hash-ref! (requires+provides-requires r+p) mpi make-hasheqv)
               phase+space-shift
               (make-hasheq)))
  (unless is-cross-phase-persistent?
    (set-requires+provides-can-cross-phase-persistent?! r+p #f))
  mpi)

;; Register a specific identifier that is required;
;; id should already have the scope of its space  (if any)
(define (add-defined-or-required-id! r+p id phase+space binding
                                     #:can-be-shadowed? [can-be-shadowed? #f]
                                     #:as-transformer? as-transformer?)
  ;; Register specific required identifier
  (unless (eqv? phase+space (phase+space+ (module-binding-nominal-phase+space binding)
                                          (module-binding-nominal-require-phase+space-shift binding)))
    (error "internal error: binding phase+space does not match nominal info"))
  (add-defined-or-required-id-at-nominal! r+p id phase+space
                                          #:nominal-module (module-binding-nominal-module binding)
                                          #:nominal-require-phase+space-shift (module-binding-nominal-require-phase+space-shift binding)
                                          #:can-be-shadowed? can-be-shadowed?
                                          #:as-transformer? as-transformer?))
                         

;; The internals of `add-defined-or-required-id!` that consumes just
;; the needed part of the binding
(define (add-defined-or-required-id-at-nominal! r+p id phase+space
                                                #:nominal-module nominal-module
                                                #:nominal-require-phase+space-shift nominal-require-phase+space-shift
                                                #:can-be-shadowed? can-be-shadowed?
                                                #:as-transformer? as-transformer?)
  (define at-mod (hash-ref! (requires+provides-requires r+p)
                            (intern-mpi r+p nominal-module)
                            make-hasheqv))
  (define sym-to-reqds (hash-ref! at-mod nominal-require-phase+space-shift make-hasheq))
  (define sym (syntax-e id))
  ;; Record that the identifier is required
  (hash-set! sym-to-reqds sym (cons-ish (required id phase+space can-be-shadowed? as-transformer?)
                                        (hash-ref sym-to-reqds sym null))))

;; Like `add-defined-or-required-id!`, but faster for bindings that
;; all have the same scope, etc., and no space level
;; Return #t if any required id is already defined by a shaodwing definition.
(define (add-bulk-required-ids! r+p s self nominal-module phase-level provides provide-phase+space
                                #:prefix bulk-prefix
                                #:excepts bulk-excepts
                                #:symbols-accum symbols-accum 
                                #:in orig-s
                                #:can-be-shadowed? can-be-shadowed?
                                #:check-and-remove? check-and-remove?
                                #:accum-update-nominals accum-update-nominals
                                #:who who)
  (define phase+space (phase+space+ provide-phase+space phase-level))
  (define phase (phase+space-phase phase+space))
  (define space (phase+space-space phase+space))
  (define s-at-space (add-space-scope s space))
  (define shortcut-table (and check-and-remove?
                              ((hash-count provides) . > . 64)
                              (syntax-mapped-names s-at-space phase)))
  (define mpi (intern-mpi r+p nominal-module))
  (define at-mod (hash-ref! (requires+provides-requires r+p) mpi make-hasheqv))
  (define sym-to-reqds (hash-ref! at-mod phase-level make-hasheq))
  (define prefix-len (if bulk-prefix (string-length (symbol->string bulk-prefix)) 0))
  (define br (bulk-required provides prefix-len s provide-phase+space can-be-shadowed?))
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
       (define bound-status
         (cond
           [(and check-and-remove?
                 (or (not shortcut-table)
                     (hash-ref shortcut-table sym #f)))
            (define id (datum->syntax s-at-space sym s-at-space))
            (adjust-shadow-requires! r+p id phase space)
            (check-not-defined #:check-not-required? #t
                               #:allow-defined? (requires+provides-definitions-shadow-imports? r+p)
                               r+p id phase space #:in orig-s
                               #:unless-matches
                               (lambda ()
                                 (provide-binding-to-require-binding binding/p
                                                                     sym
                                                                     #:self self
                                                                     #:mpi mpi
                                                                     #:provide-phase+space provide-phase+space
                                                                     #:phase+space-shift phase-level))
                               #:remove-shadowed!? #t
                               #:accum-update-nominals accum-update-nominals
                               #:who who)]
           [else #f]))
       (define already-defined? (eq? bound-status 'defined))
       (unless already-defined?
         (hash-set! sym-to-reqds sym (cons-ish br (hash-ref sym-to-reqds sym null))))
       (or any-already-defined? already-defined?)])))

;; Convert a combination of a symbol and `bulk-required` to a
;; `required` on demand
(define (bulk-required->required br nominal-module phase+space-shift sym)
  (define prefix-len (bulk-required-prefix-len br))
  (define out-sym (if (zero? prefix-len)
                      sym
                      (string->symbol (substring (symbol->string sym) prefix-len))))
  (define binding/p (hash-ref (bulk-required-provides br) out-sym))
  (define phase+space (phase+space+ (bulk-required-provide-phase+space br) phase+space-shift))
  (required (add-space-scope (datum->syntax (bulk-required-s br) sym)
                             (phase+space-space phase+space))
            phase+space
            (bulk-required-can-be-shadowed? br)
            (provided-as-transformer? binding/p)))

(define (normalize-required r mod-name phase+space-shift sym)
  (if (bulk-required? r)
      (bulk-required->required r mod-name phase+space-shift sym)
      r))

;; Add bindings of an enclosing module
(define (add-enclosing-module-defined-and-required! r+p
                                                    #:enclosing-requires+provides enclosing-r+p
                                                    enclosing-mod
                                                    phase-level)
  (set-requires+provides-all-bindings-simple?! r+p #f)
  (for ([(mod-name at-mod) (in-hash (requires+provides-requires enclosing-r+p))])
    (for* ([(phase+space-shift at-phase) (in-hash at-mod)]
           [(sym reqds) (in-hash at-phase)]
           [reqd/maybe-bulk (in-list-ish reqds)])
      (define reqd (normalize-required reqd/maybe-bulk mod-name phase+space-shift sym))
      (add-defined-or-required-id-at-nominal! r+p
                                              (syntax-shift-phase-level
                                               (syntax-module-path-index-shift 
                                                (required-id reqd)
                                                (requires+provides-self enclosing-r+p)
                                                enclosing-mod)
                                               phase-level)
                                              (phase+space+ (required-phase+space reqd) phase-level)
                                              #:nominal-module enclosing-mod
                                              #:nominal-require-phase+space-shift phase-level
                                              #:can-be-shadowed? #t
                                              #:as-transformer? (required-as-transformer? reqd)))))

;; Removes a required identifier, in anticipation of it being defined.
;; The `check-not-defined` function below is similar, and it also includes
;; an option to remove shadowed bindings. Note that `id` may have a space scope.
(define (remove-required-id! r+p id phase)
  (define (remove! id #:bind-as-ambiguous? [bind-as-ambiguous? #f])
    (define b (resolve+shift id phase #:exactly? #t))
    (when (and (module-binding? b)
               (not (eq? (requires+provides-self r+p)
                         (module-binding-module b))))
      (when bind-as-ambiguous?
        (add-binding! id (like-ambiguous-binding) phase))
      (define mpi (intern-mpi r+p (module-binding-nominal-module b)))
      (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
      (when at-mod
        (define nominal-phase+space-shift (module-binding-nominal-require-phase+space-shift b))
        (define sym-to-reqds (hash-ref at-mod
                                       nominal-phase+space-shift
                                       #f))
        (when sym-to-reqds
          (define sym (syntax-e id))
          (define l (hash-ref sym-to-reqds sym null))
          (unless (null? l)
            (hash-set! sym-to-reqds sym (remove-non-matching-requireds l id phase mpi nominal-phase+space-shift sym)))))))
  ;; normal remove step:
  (remove! id)
  ;; try each space to implement shadowing definitions that, like local definitions,
  ;; should trigger ambiguous-binding errors in other spaces; if `id` already has
  ;; a space, adding an extra space should still be ok and consistent with local binding
  (for ([space (in-hash-keys (requires+provides-spaces r+p))])
    (remove! (add-space-scope id space) #:bind-as-ambiguous? #t)))

;; Prune a list of `required`s to remove any with a different binding
(define (remove-non-matching-requireds reqds id phase mpi nominal-phase+space-shift sym)
  ;; Ok to produce a list-ish instead of a list, but we don't have `for*/list-ish`:
  (for*/list ([r (in-list-ish reqds)]
              [r (in-value (normalize-required r mpi nominal-phase+space-shift sym))]
              #:unless (and (eqv? phase (phase+space-phase (required-phase+space r)))
                            (free-identifier=? (required-id r) id phase phase)))
    r))

;; Check whether an identifier has a binding that is from a non-shadowable
;; require; if something is found but it will be replaced, then record that
;; bindings are not simple. Returns a status to indicate whether/how the binding
;; is defined or required already, since `allow-defined?` and `ok-binding/delated`
;; allow that possibilify; the possible results are #f, 'defined, or 'required.
(define (check-not-defined #:check-not-required? [check-not-required? #f]
                           #:allow-defined? [allow-defined? #f]
                           r+p id phase space #:in orig-s
                           #:unless-matches [ok-binding/delayed #f] ; binding or (-> binding)
                           #:remove-shadowed!? [remove-shadowed!? #f]
                           #:accum-update-nominals [accum-update-nominals #f]
                           #:who who)
  (define b (resolve+shift id phase #:exactly? #t))
  (define (check-default-space)
    (cond
      [(or (not space) (not check-not-required?)) #f]
      [else
       (define default-b (resolve+shift (remove-space-scope id space) phase #:exactly? #t))
       (define defined? (and default-b (eq? (requires+provides-self r+p)
                                            (module-binding-module default-b))))
       (cond
         [(and defined?)
          ;; Since the default space has a definition, we want to treat a
          ;; require into a non-default space as ambiguous, which simulates
          ;; the ambiguity that happens with a shadowing local binding
          (add-binding! id (like-ambiguous-binding) phase)
          'defined]
         [else #f])]))
  (cond
   [(not b) (check-default-space)]
   [(not (module-binding? b))
    (cond
      [allow-defined?
       ;; A local binding shadows this would-be `require`, which is
       ;; possible and sensible if a require is lifted without a new
       ;; scope. This is not sensible if some scope has been put onto a
       ;; `require` form out-of-context, but it seems that we can't help
       ;; report the non-sensible configuration without disallowing the
       ;; sensible one.
       (set-requires+provides-all-bindings-simple?! r+p #f)
       'defined]
      [else
       (raise-syntax-error #f "identifier out of context" id)])]
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
       (check-default-space)]
      [else
       (define define-shadowing-require? (and (not defined?)
                                              (requires+provides-definitions-shadow-imports? r+p)
                                              (not check-not-required?)))
       (define mpi (intern-mpi r+p (module-binding-nominal-module b)))
       (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
       (define ok-binding (and (not define-shadowing-require?)
                               (if (procedure? ok-binding/delayed)
                                   (ok-binding/delayed)
                                   ok-binding/delayed)))
       (define (raise-already-bound defined? where)
         (raise-syntax-error who
                             (string-append "identifier"
                                            (cond
                                              [(and (not defined?) (not check-not-required?))
                                               " for definition"]
                                              [(and defined? check-not-required?)
                                               " for require"]
                                              [else ""])
                                            " already "
                                            (if defined? "defined" "required")
                                            (cond
                                              [(zero-phase? phase) ""]
                                              [(label-phase? phase) " for label"]
                                              [(= 1 phase) " for syntax"]
                                              [else (format " for phase ~a" phase)]))
                             orig-s
                             id
                             null
                             (cond
                               [(bulk-required? where)
                                (format "\n  also provided by: ~.s" (syntax->datum (bulk-required-s where)))]
                               [else ""])))
       (cond
         [(and (not at-mod)
               (not define-shadowing-require?))
          ;; Binding is from an enclosing context; if it's from an
          ;; enclosing module, then we've already marked bindings
          ;; a non-simple --- otherwise, we don't care
          (check-default-space)]
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
          (if defined? 'defined 'required)]
         [(and defined? allow-defined?)
          ;; A `require` doesn't conflict with a definition, even if we
          ;; saw the definition earlier; but make sure there are not multiple
          ;; `require`s (any one of which would be shadowed by the definition)
          (define also-required (requires+provides-also-required r+p))
          (define prev-b (hash-ref also-required (module-binding-sym b) #f))
          (when (and prev-b (not (same-binding? ok-binding prev-b)))
            (raise-already-bound #f #f))
          (hash-set! also-required (module-binding-sym b) ok-binding)
          (set-requires+provides-all-bindings-simple?! r+p #f)
          'defined]
         [else
          ;; We're shadowing a binding, either through a definition or a require
          (define nominal-phase+space-shift (module-binding-nominal-require-phase+space-shift b))
          (define sym-to-reqds (hash-ref at-mod nominal-phase+space-shift #hasheq()))
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
                [else (raise-already-bound defined? r)])))
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
                          (remove-non-matching-requireds reqds id phase mpi nominal-phase+space-shift (syntax-e id))))])
          #f])])]))

;; For importing into the default space, adjust shadowable imports of
;; the same name into non-default spaces to that they're treated as
;; abiguous, the same as would happen for local bindings.
(define (adjust-shadow-requires! r+p id phase space)
  (unless space
    (for ([space (in-hash-keys (requires+provides-spaces r+p))])
      ;; Check binding in `space`
      (define space-id (add-space-scope id space))
      (define b (resolve+shift space-id phase #:exactly? #t))
      (when (and (module-binding? b)
                 (not (eq? (requires+provides-self r+p)
                           (module-binding-module b))))
        ;; Currently bound by require; by only a shadowable require?
        (define mpi (intern-mpi r+p (module-binding-nominal-module b)))
        (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
        (define nominal-phase+space-shift (module-binding-nominal-require-phase+space-shift b))
        (define sym-to-reqds (hash-ref at-mod nominal-phase+space-shift #hasheq()))
        (define reqds (hash-ref sym-to-reqds (syntax-e id) null))
        (when (for/and ([r (in-list-ish reqds)])
                (if (bulk-required? r)
                    (bulk-required-can-be-shadowed? r)
                    (required-can-be-shadowed? r)))
          ;; It's a shadowable require, so we want to make it act ambiguous
          ;; and treat it as no longer imported
          (hash-set! sym-to-reqds (syntax-e id)
                     (remove-non-matching-requireds reqds space-id phase mpi nominal-phase+space-shift (syntax-e id)))
          (add-binding! space-id (like-ambiguous-binding) phase))))))
        

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
;; that was required with a given phase and space level
(define (extract-module-requires r+p mod-name phase+space-shift)
  (define mpi (intern-mpi r+p mod-name))
  (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
  (and at-mod
       (for*/list ([(sym reqds) (in-hash (hash-ref at-mod phase+space-shift #hasheq()))]
                   [reqd (in-list-ish reqds)])
         (normalize-required reqd mpi phase+space-shift sym))))

;; Get all the definitions
(define (extract-module-definitions r+p)
  (or (extract-module-requires r+p (requires+provides-self r+p) 0)
      null))

;; Like `extract-module-requires`, but merging modules and phases
(define (extract-all-module-requires r+p
                                     mod-name ; or #f for "all"
                                     phase+space-shift)   ; or 'all for "all"
  (define self (requires+provides-self r+p))
  (define requires (requires+provides-requires r+p))
  (let/ec esc
    (for*/list ([mod-name (in-list (if mod-name
                                       (list (intern-mpi r+p mod-name))
                                       (hash-keys requires)))]
                #:unless (eq? mod-name self)
                [phase+space-to-requireds (in-value (hash-ref requires mod-name #hasheqv()))]
                [phase+space-shift (in-list (if (eq? phase+space-shift 'all)
                                                (hash-keys phase+space-to-requireds)
                                                (list phase+space-shift)))]
                [(sym reqds) (in-hash
                              (hash-ref phase+space-to-requireds phase+space-shift
                                        ;; failure => not required at that phase+space
                                        (lambda () (esc #f))))]
                [reqd (in-list-ish reqds)])
      (normalize-required reqd mod-name phase+space-shift sym))))

(define (disable-definitions-shadow-imports! r+p)
  (set-requires+provides-definitions-shadow-imports?! r+p #f))

;; ----------------------------------------

;; Register that a binding is provided as a given symbol; report an
;; error if the provide is inconsistent with an earlier one
(define (add-provide! r+p sym phase+space binding immed-binding id orig-s
                      #:as-protected? as-protected?
                      #:as-transformer? as-transformer?)
  (when (and as-protected?
             (not (eq? (module-binding-module immed-binding) (requires+provides-self r+p))))
    (raise-syntax-error #f "cannot protect required identifier in re-provide" sym))
  (hash-update! (requires+provides-provides r+p)
                phase+space
                (lambda (at-phase)
                  (define b/p (hash-ref at-phase sym #f))
                  (define b (provided-as-binding b/p))
                  (cond
                   [(not b)
                    ;; Record this binding, but first strip away any `free-identifier=?`
                    ;; identifier that remains, which means that it doesn't have a binding.
                    ;; The serializer and deserializer won't be able to handle that, and
                    ;; it's not relevant to further comparisons.
                    (define plain-binding
                      (if (binding-free=id binding)
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
  (define (extract-requires #:recurs? [recurs? #f])
    ;; Extract from the in-order record, so that instantiation can use the original order
    (define phase-to-mpis-in-order (requires+provides-require-mpis-in-order r+p))
    (define phases-in-order (sort (hash-keys phase-to-mpis-in-order) phase<?))
    ;; If a resolved module path is in `transitive-requires`, then it's required by
    ;; one of the modules that is required, so we won't need to redundantly check
    ;; instantiation at run time
    (define transitive-requires (requires+provides-transitive-requires r+p))
    (for/list ([phase (in-list phases-in-order)])
      (define elems (for/list ([mpi (in-list (reverse (hash-ref phase-to-mpis-in-order phase)))]
                               #:unless (eq? mpi old-self))
                      (if recurs?
                          (not (hash-ref (hash-ref transitive-requires
                                                   (module-path-index-resolved mpi)
                                                   #hasheqv())
                                         phase
                                         #f))
                          (module-path-index-shift mpi old-self new-self))))
      (if recurs?
          elems
          (cons phase elems))))
  (define (extract-provides)
    (shift-provides-module-path-index (requires+provides-provides r+p)
                                      old-self
                                      new-self))
  (values (extract-requires) (extract-requires #:recurs? #t) (extract-provides)))

;; ----------------------------------------

;; Note: the provides may include non-interned symbols. Those may be
;; accessible via` dynamic-require`, but don't import them.
(define (shift-provides-module-path-index provides from-mpi to-mpi)
  (for/hasheqv ([(phase+space at-phase) (in-hash provides)])
    (values phase+space
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
