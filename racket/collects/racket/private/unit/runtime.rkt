#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/transformer
                     syntax/parse/pre
                     "exptime/signature.rkt"
                     "exptime/syntax.rkt")
         racket/contract
         (only-in racket/contract/private/blame make-blame)
         racket/unsafe/undefined
         syntax/location
         syntax/srcloc)

(provide (rename-out [make-a-unit make-unit]) unit? unit-import-sigs unit-export-sigs unit-go unit-deps
         unit-export check-unit check-no-imports check-sigs check-deps check-helper

         (for-syntax build-install-export!-expr
                     build-unbox-import-expr
                     unit-variable-binding?
                     make-export-binding
                     make-import-binding
                     make-unbox-import-binding))

;; for named structures
(define insp (current-inspector))

#| Note [Signature runtime representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At runtime, signatures largely do not exist: they are quite
second-class. But we do need a unique value to identify each
signature by, as the linker needs to be able to match up linkages
by their signature. No other information is necessary, so a gensym
per signature will do; we call this gensym the *signature id*.

However, two things complicate the story slightly:

  1. Multiple linkages for the same signature can be distinguished
     using tags.

  2. Signatures support inheritance, which allows an import linkage
     to be satisfied by an export linkage of a sub-signature.

The first point is managed easily by possibly combining each
signature gensym with a tag symbol, yielding the definition of a
*signature key*:

    signature-id?  = (and/c symbol? symbol-uninterned?)
    tag-symbol?    = (and/c symbol? symbol-interned?)
    signature-key? = (or/c signature-id?
                           (cons/c tag-symbol? signature-key?))

The second point is somewhat more subtle, but our solution is
simple: each signature is associated with a *list* of signature ids,
and a sub-signature includes the signature ids of all of its super-
signatures. Each signature id uniquely identifies the new bindings
introduced by *that particular* signature, and those bindings can
be stored in separate vectors and linked largely independently.

Note [The unit invocation and linking protocol]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unit invocation is a two-step process to support dynamic linking.

  1. In the first phase, each unit’s `unit-go` thunk is applied,
     which allocates and returns an *export table* for each of the
     unit’s exported signatures, plus a procedure to be invoked in
     the second phase.

  2. In the second phase, an *import table* is assembled for each
     unit from the export tables the unit is linked against, and
     the procedure returned in the first phase is applied to this
     import table to execute the unit body.

Each of these tables has the following shape:

    ie-table/c = (hash/c signature-key?
                         (immutable-vectorof (-> any/c)))

Each signature key is mapped to a vector that contains a thunk for
each binding exported by the signature, which returns the binding’s
current value. This thunking is needed because the binding might be
uninitialized: each thunk wraps the access to the binding with
`check-not-unsafe-undefined`, so the importing unit does not need to
perform the check.

For signature bindings with contracts, the linking protocol is
slightly more involved; see Note [Linking for contracted imports
and exports] for the full details.

Note [Linking for contracted imports and exports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Signature bindings with contracts complicate the linking protocol in
two ways:

  1. Assignment of negative blame depends on the unit that imports
     the binding (and there many be multiple such units).

  2. Contract expressions can depend on the value of signature
     members, so evaluating them too early can trigger use-before-
     initialization errors.

The first point is handled by simply adding another layer of
indirection: rather than returning the value directly, each binding’s
thunk returns a procedure of one argument that accepts the negative
blame party and returns the contracted value.

The second point is more subtle. It may seem like we ought to be able
to simply delay evaluating the contract expressions until the unit is
fully initialized, but this can cause values to be initialized too late
when recursive linking is involved. Consider the following program:

    (define-signature a^
      [add-n])
    (define-signature b^
      [(contracted [n number?]
                   [m number?])])

    (define-unit a@
      (import b^)
      (export a^)

      (define (add-n x)
        (+ x n)))

    (define-unit b@
      (import a^)
      (export b^)
      (init-depend a^)

      (define n 5)
      (define m (add-n 10)))

    (invoke-unit/infer (link a@ b@))

This program should run successfully, but if we wait to attach contracts
until `b@` is fully initialized, then the call to `add-n` will fail when
it tries to evaluate `n`. On the other hand, we cannot simply attach
contracts immediately, as that would cause examples like this to fail:

    (define-signature c^
      [(contracted
        [p? predicate/c]
        [f (-> p? p?)])])

    (define-unit c@
      (import)
      (export c^)

      (define f add1)
      (define p? number?))

We could demand that the user manually ensure that definitions in a
unit are initialized in such an order that this never causes trouble,
but this is difficult to reason about, especially when the author of a
signature and a unit that implements it are not the same person.

To accept both these programs, we accept a balance between the two
approaches: a contracted export is initialized as soon as its
corresponding definition is evaluated, but the contract expression
itself is evaluated lazily. When the export is evaluated for the first
time, we evaluate the contract and cache the resulting projection, so
subsequent uses do not incur any additional overhead.

Note [Preserve import laziness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When recursive linking is involved, it becomes quite crucial that we
do not evaluate unit imports too early, as this can cause use-before-
initialization errors. We must therefore be exceedingly careful when
compiling forms that build new units by wrapping existing units, such
as `unit/new-import-export` and `unit/c`. Consider the following example:

    (define-signature a^ [x])
    (define-signature b1^ [get-x])

    (define-unit a@
      (import)
      (export a^)
      (define x 1))

    (define-unit b1@
      (import a^)
      (export b1^)
      (define (get-x)
        x))

Given these definitions, it is perfectly legal to link the units in
either order, since although `b1@` imports `a^`, its initialization
does not evaluate `x`. Now suppose we use `unit/new-import-export`
to change the signature exported by `b1@`:

    (define-signature b2^ [get-x])
    (define-unit/new-import-export b2@
      (import a^)
      (export b2^)
      b1@)

It must *also* be legal to link `a@` and `b2@` in either order.
Ensuring we do not break this takes some care, as
`unit/new-import-export` essentially expands to the following:

    (define-unit b2@
      (import a^)
      (export b2^)
      (define-values/invoke-unit b1@
        (import a^)
        (export b1^))

In turn, `define-values/invoke-unit` desugars its `import` clause to
linking with a unit built from the current context:

    (define-unit b2@
      (import a^)
      (export b2^)
      (define-values/invoke-unit
        (compound-unit
          (import)
          (export B1)
          (link [([A : a^]) (unit-from-context a^)]
                [([B1 : b^]) b1@ A]))
        (export b1^)))

If `unit-from-context` evaluates its exports when building its export
table, it will evaluate `b2@`’s imports, but that’s precisely what we
must not do. So, somehow, we need to be lazier. However, we don’t
want to be *too* lazy. For example, if we were to have

    (let-syntax ([x (λ (stx) #'(launch-the-missiles!))])
      (unit-from-context a^))

then the programmer would presumably be unhappy if invoking the
resulting unit did not actually launch the missiles, and they would be
even more unhappy if they were launched multiple times. Therefore,
in general, we really do need to eagerly bind fresh locations.

However, there is a way out of this conundrum: in the above example,
we know it is safe to defer evaluation of unit imports, and we know it
is safe to evaluate them multiple times. Since we control the binding
sites for imported identifiers, we can make `unit-from-context`
cooperate with `import` by exporting imported variables directly. To
implement this, we bind each imported variable (and exported variable,
since it doesn’t hurt) to a compile-time `unit-variable-binding?`
value. When `unit-from-context` detects that an export is either a
genuine variable or one of these unit bindings, it references it
directly in the generated export thunk, avoiding unnecessary evaluation.

This is all quite subtle, but it greatly simplifies the implementation
of forms like `unit/new-import-export`, and it has the added advantage
that user-defined abstractions that wrap units will automatically
benefit as well. |#

;; Runtime representation of a unit
(define-struct unit
  ;; Note: the additional symbols in `import-sigs` and `export-sigs`
  ;; are the symbolic names of the signatures, for error reporting.
  (import-sigs ; (vectorof (cons/c symbol? (vectorof signature-key?)))
   export-sigs ; (vectorof (cons/c symbol? (vectorof signature-key?)))
   deps        ; (listof signature-key?)
   go))        ; (-> (values (-> import-table any) export-table ...))

;; For units with inferred names, generate a struct that prints using the name:
(define (make-naming-constructor type name)
  (let-values ([(struct: make- ? -accessor -mutator)
                (make-struct-type name type 0 0 #f null insp)])
    make-))

;; Make a unit value (call by the macro expansion of `unit')
(define (make-a-unit name num-imports exports deps go)
  ((if name
       (make-naming-constructor
        struct:unit
        (string->symbol (format "unit:~a" name)))
       make-unit)
   num-imports exports deps go))

;; Helper for building the export table
(define-syntax (unit-export stx)
  (syntax-parse stx
    [(_ ((key-e:expr ...) vals-e:expr) ...)
     (define/syntax-parse [vals-id ...] (generate-temporaries (attribute vals-e)))

     (define/syntax-parse (([k . v] ...) ...)
       (for/list ([key-es (in-list (attribute key-e))]
                  [vals-id (in-list (attribute vals-id))])
         (for/list ([key-e (in-list key-es)])
           (cons key-e vals-id))))

     #'(let ([vals-id vals-e] ...)
         (hash {~@ k v} ... ...))]))

;; check-unit : X symbol ->
;; ensure that u is a unit value
(define (check-unit u name)
  (unless (unit? u)
    (raise
     (make-exn:fail:contract
      (format "~a: result of unit expression was not a unit: ~e" name u)
      (current-continuation-marks)))))

;; check-helper : (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;                 (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;;                symbol symbol ->
;; ensure that the unit's signatures match the expected signatures.
(define (check-helper sub-sig super-sig name import?)
  (define t (make-hash))
  (let loop ([i (sub1 (vector-length sub-sig))])
    (when (>= i 0)
      (let ([v (cdr (vector-ref sub-sig i))])
        (let loop ([j (sub1 (vector-length v))])
          (when (>= j 0)
            (let ([vj (vector-ref v j)])
              (hash-set! t vj
                         (if (hash-ref t vj #f)
                             'amb
                             #t)))
            (loop (sub1 j)))))
      (loop (sub1 i))))
  (let loop ([i (sub1 (vector-length super-sig))])
    (when (>= i 0)
      (let* ([v0 (vector-ref (cdr (vector-ref super-sig i)) 0)]
             [r (hash-ref t v0 #f)])
        (when (or (eq? r 'amb) (not r))
          (let ([tag (if (pair? v0) (car v0) #f)]
                [sub-name (car (vector-ref super-sig i))]
                [err-str (if r
                             "supplies multiple times"
                             "does not supply")])
            (raise
             (make-exn:fail:contract
              (cond
                [(and import? tag)
                 (format "~a: unit argument expects an import for tag ~a with signature ~a, which this usage context ~a"
                         name
                         tag
                         sub-name
                         err-str)]
                [import?
                 (format "~a: unit argument expects an untagged import with signature ~a, which this usage context ~a"
                         name
                         sub-name
                         err-str)]
                [tag
                 (format "~a: this usage context expects a unit with an export for tag ~a with signature ~a, which the given unit ~a"
                         name
                         tag
                         sub-name
                         err-str)]
                [else
                 (format "~a: this usage context expects a unit with an untagged export with signature ~a, which the given unit ~a"
                         name
                         sub-name
                         err-str)])
              (current-continuation-marks))))))
      (loop (sub1 i)))))

;; check-deps : (hash/c signature-key? (cons/c symbol? symbol?)) unit? symbol? -> void?
;; The hash table keys are signature keys (see Note [Signature runtime representation]).
;; The values are the name of the signature and the link-id.
(define (check-deps dep-table unit name)
  (for ([dep-key (in-list (unit-deps unit))])
    (define r (hash-ref dep-table dep-key #f))
    (when r
      (raise
       (make-exn:fail:contract
        (if (pair? dep-key)
            (format "~a: initialization dependent signature ~a with tag ~a is supplied from a later unit with link ~a"
                    name (car r) (car dep-key) (cdr r))
            (format "~a: untagged initialization dependent signature ~a is supplied from a later unit with link ~a"
                    name (car r) (cdr r)))
        (current-continuation-marks))))))

;; check-no-imports : unit symbol ->
;; ensures that the unit has no imports
(define (check-no-imports unit name)
  (check-helper (vector) (unit-import-sigs unit) name #t))

;; check-sigs : unit
;;              (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;;              (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;;              symbol ->
;; ensures that unit has the given signatures
(define (check-sigs unit expected-imports expected-exports name)
  (check-helper expected-imports (unit-import-sigs unit) name #t)
  (check-helper (unit-export-sigs unit) expected-exports name #f))

;; install-export-contract-projection!
;;   : contract? (-> any/c) any/c any/c source-location? -> (any/c -> any/c)
;;
;; Installs a procedure for a contracted export into the given box. The procedure
;; accepts a negative blame party and returns the contracted value. The contract
;; expression is evaluated lazily to avoid use-before-initialization errors;
;; see Note [Linking for contracted imports and exports] for details.
(define (install-export-contract-projection! export-box get-ctc get-val pos name loc)
  (set-box! export-box
   (λ (neg)
     ;; If evaluation of the contract expression somehow depends on the
     ;; contracted value, raise a use-before-initialization error to
     ;; avoid infinitely looping.
     (set-box! export-box (λ (neg) (check-not-unsafe-undefined unsafe-undefined name)))

     (define ctc (coerce-contract 'contracted (get-ctc)))
     (define blame (make-blame (build-source-location loc)
                               name
                               (λ () (contract-name ctc))
                               pos
                               #f
                               #t))
     (define neg-proj (with-contract-continuation-mark blame
                        ((contract-late-neg-projection ctc) blame)))

     (define (forced-proj neg)
       (define val (get-val))
       (with-contract-continuation-mark (cons blame neg)
         (neg-proj val neg)))

     (set-box! export-box forced-proj)
     (forced-proj neg))))

(begin-for-syntax
  ;; build-install-export!-expr : syntax? syntax? identifier? (or/c syntax? #f) -> syntax?
  ;;
  ;; Builds an expression that installs an exported value into its export box,
  ;; wrapping it with a contract projection if necessary.
  ;; See Note [Linking for contracted imports and exports] for details.
  (define (build-install-export!-expr box-expr val-expr name-id ctc-expr)
    (if ctc-expr
        #`(install-export-contract-projection!
           #,box-expr
           (λ () #,ctc-expr)
           (λ () #,val-expr)
           (current-contract-region)
           (quote #,name-id)
           (quote-srcloc #,ctc-expr))
        #`(set-box! #,box-expr #,val-expr)))

  ;; build-unbox-import-expr : syntax? identifier? any/c -> syntax?
  ;;
  ;; Builds an expression that unboxes an imported value, applying the
  ;; contract projection if necessary.
  ;; See Note [Linking for contracted imports and exports] for details.
  (define (build-unbox-import-expr src-stx loc-id contracted?)
    (if contracted?
        (quasisyntax/loc src-stx
          (#,(quasisyntax/loc src-stx
               (#,loc-id))
           (current-contract-region)))
        (quasisyntax/loc src-stx
          (#,loc-id))))

  ;; A `unit-variable-binding?` is essentially just a `set!-transformer?`,
  ;; but it identifies the binding as an imported or exported unit
  ;; variable, which is useful for implementing certain forms. See
  ;; Note [Preserve import laziness] for details.
  (struct unit-variable-binding (procedure)
    #:property prop:set!-transformer (struct-field-index procedure))

  (define (make-unit-variable-binding val-expr #:import? import?)
    (unit-variable-binding
     (set!-transformer-procedure
      (make-variable-like-transformer
       val-expr
       (λ (stx)
         (raise-syntax-error 'unit
                             (format "cannot set! ~a variable"
                                     (if import? "imported" "exported"))
                             stx))))))

  (define (make-export-binding val-expr)
    (make-unit-variable-binding val-expr #:import? #f))

  (define (make-import-binding val-expr)
    (make-unit-variable-binding val-expr #:import? #t))

  ;; The common case of `make-import-binding`, where the binding’s
  ;; value comes from unboxing a real unit import.
  (define (make-unbox-import-binding loc-id contracted?)
    (define contracted?* (and contracted? #t))
    (make-import-binding
     (λ (stx)
       (build-unbox-import-expr stx loc-id contracted?*)))))
