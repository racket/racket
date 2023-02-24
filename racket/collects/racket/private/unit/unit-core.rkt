#lang racket/base

;; This module implements the core unit forms, without support for inferred linking.

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse/pre
                     syntax/context
                     syntax/private/id-table
                     syntax/intdef
                     syntax/kerncase
                     syntax/name
                     syntax/stx
                     syntax/transformer
                     "exptime/import-export.rkt"
                     "exptime/signature.rkt"
                     "exptime/syntax.rkt")
         racket/contract/base
         racket/contract/region
         racket/list
         racket/stxparam
         racket/unsafe/undefined
         syntax/location
         "keywords.rkt"
         "runtime.rkt"
         "util.rkt")

(provide (for-syntax build-unit
                     build-unit-from-context
                     build-unit/new-import-export
                     build-compound-unit
                     invoke-results-clause)
         unit
         unit-from-context
         unit/new-import-export
         compound-unit
         invoke-unit
         define-values/invoke-unit)

;; -----------------------------------------------------------------------------
;; `unit`

(begin-for-syntax
  (define (localify exp def-ctx)
    (internal-definition-context-introduce def-ctx exp 'add))

  ;; build-unit : syntax-object -> 
  ;;             (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit expression.  stx must be
  ;; such that it passes check-unit-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define (build-unit
           stx
           #:contract-region? [contract-region? #t]
           ; see Note [Suppress generated exports on reexport]
           #:suppress-generated-exports? [suppress-generated-exports? #f])
    (syntax-parse stx
      #:context (current-syntax-context)
      #:literals [import export init-depend]
      [[(import in:tagged-import-spec ...)
        (export out:tagged-export-spec ...)
        deps:opt-init-depends
        . body]

       ;; INTRODUCED FORMS AND MACROS:
       ;; We need to distinguish the original body from any
       ;; forms that are introduced from signatures 
       ;; (via `define-values`, etc., in a signature body).
       ;; The `intro` mark should be added to everything except
       ;; the introduced parts, which we implement by adding the
       ;; mark to the introduced parts and then flipping it
       ;; everywhere.
       (define intro (make-syntax-introducer #t))

       (define unit-expr
         (build-make-unit
          #:contract-region? contract-region?
          #:imports (attribute in.value)
          #:exports (attribute out.value)
          #:init-deps (attribute deps.dep)
          (λ ()
            (define/syntax-parse ([in-loc ...] ...) (map generate-temporaries (attribute in.var.int-id)))
            (define/syntax-parse ([out-loc ...] ...) (map generate-temporaries (attribute out.var.int-id)))
            (define/syntax-parse [in-count ...] (for/list ([vars (in-list (attribute in.var.int-id))])
                                                  (length vars)))
            (define/syntax-parse ([in-ctc? ...] ...) (for/list ([ctcs (attribute in.ctc)])
                                                       (for/list ([ctc (in-list ctcs)])
                                                         #`(quote #,(and ctc #t)))))

            (define/syntax-parse ([rename-bind [stx-bind ...] [val-bind ...]] ...)
              (map (build-val+macro-defs intro) (attribute in.value)))

            (define/syntax-parse ([[post-rhs ...] [out-ctc ...]] ...)
              (for/list ([sig (attribute out.value)])
                (define-values [post-rhss ctcs] (build-post-val-defs+ctcs sig))
                (list post-rhss
                      (for/list ([ctc (in-list ctcs)])
                        ; see Note [Passing contracts to unit-body]
                        (and ctc (box-immutable ctc))))))

            (quasisyntax/loc (current-syntax-context)
              (lambda ()
                (let ([out-loc (box unsafe-undefined)] ... ...)
                  (values
                   #,(quasisyntax/loc (current-syntax-context)
                       (lambda (import-table)
                         (let-values ([(in-loc ...) (vector->values (hash-ref import-table in.self-key) 0 'in-count)] ...)
                           (letrec-syntaxes ([(in.var.int-id ...)
                                              (values (make-unbox-import-binding (quote-syntax in-loc) in-ctc?)
                                                      ...)]
                                             ...)
                             (letrec-syntaxes+values (rename-bind ... stx-bind ... ...)
                               (val-bind ... ...)
                               (unit-body #,(current-syntax-context)
                                          [in.var.int-id ... ...]
                                          ([out.var.int-id out.var.ext-id out-loc out-ctc] ... ...)
                                          (begin . body)
                                          #,@(if suppress-generated-exports?
                                                 #'[]
                                                 #'[(define-values [out.post-def.id ...] post-rhs) ... ...])))))))
                   (unit-export ([out.key ...]
                                 (vector-immutable (λ () (check-not-unsafe-undefined (unbox out-loc) 'out.var.ext-id))
                                                   ...))
                                ...))))))))

       (values (syntax-parse-track-literals (syntax-protect (intro unit-expr)))
               (map cons (attribute in.tag-sym) (attribute in.sig-id))
               (map cons (attribute out.tag-sym) (attribute out.sig-id))
               (map cons (attribute deps.tag-sym) (attribute deps.sig-id)))]))

  ;; build-make-unit : (-> syntax?)
  ;;                   #:imports (listof signature-ie?)
  ;;                   #:exports (listof signature-ie?)
  ;;                   #:init-deps (listof syntax?)
  ;;                  [#:contract-region? any/c]
  ;;                -> syntax?
  ;; Builds a `make-unit` expression with the given imports, exports, and
  ;; dependencies, then calls the given thunk to build the actual unit body.
  (define (build-make-unit build-body
                           #:imports in-sigs
                           #:exports out-sigs
                           #:init-deps dep-stxs
                           #:contract-region? [contract-region? #t])
    (define/syntax-parse
      {~and {~bind-signature-ie {in 1} in-sigs}
            {~bind-signature-ie {out 1} out-sigs}
            [dep:tagged-signature-id ...]} dep-stxs)

    (check-duplicate-sigs (map cons (attribute in.tag-sym) (attribute in.info)) (attribute in.src-stx)
                          (map cons (attribute dep.tag-sym) (attribute dep.info)) dep-stxs)
    (check-duplicate-subs (map cons (attribute out.tag-sym) (attribute out.info)) (attribute out.src-stx))
    (check-unit-ie-sigs in-sigs out-sigs)

    (define/syntax-parse name (syntax-local-infer-name (current-syntax-context)))

    (quasisyntax/loc (current-syntax-context)
      (make-unit
       'name
       (vector-immutable (cons 'in.info.self-id (vector-immutable in.key ...)) ...)
       (vector-immutable (cons 'out.info.self-id (vector-immutable out.key ...)) ...)
       (list dep.self-key ...)
       (syntax-parameterize #,(if contract-region?
                                  #'([current-contract-region (lambda (stx) #'(quote (unit name)))])
                                  #'())
         #,(build-body))))))

#| Note [Suppress generated exports on reexport]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Generated export definitions] in
"exptime/signature.rkt", we normally always generate export definitions
in any unit that exports the signature. However, forms like `unit/c`
expand to a wrapper unit that simply invokes a subunit in its body and
reexports the resulting bindings. These subunits already include the
generated definitions, some of those definitions may in fact be exported,
so we don’t want to generate them again.

We therefore explicitly suppress generation of export definitions for
these forms by passing `#:suppress-generated-exports? #t` to
`build-unit`. This can have slightly surprising results if the export
definitions are not actually exported, such as in the following example:

  (define-signature sig^
    [value
     (define-values-for-export [ctc] any/c)])

  (unit/c
   (import)
   (export (sig^ [value ctc])))

This program is rejected, since the use of `ctc` inside `unit/c` is
unbound. We could avoid this by being more discerning about which
definitions we suppress, continuing to generate any definitions that
do not conflict with the unit’s exports. However, this is the way the
implementation has historically worked, and nobody has complained, so
we continue to do the simple thing for now.

Note [No generated exports for unit/new-import-export]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, the `unit/new-import-export` form does not include generated
export definitions. This can be somewhat unintuitive, as `unit-from-context`
*does* generate these definitions, even though it also does not have an
explicit unit body. We suppress the definitions for two reasons:

1. The behavior is useful when some signatures are shared between
   the original exports and the new exports, since in that case, the
   form is just reexporting those signatures and the reasoning in
   Note [Suppress generated exports on reexport] applies.

2. It’s how `unit/new-import-export` has historically worked, so
   changing its behavior would be a backwards compatibility concern.

Note [Passing contracts to unit-body]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we trampoline from `unit` to `unit-body`, we need to pass along
the contract expressions from the exported signatures. However, we must
be careful: the absence of a contract is represented by #f, and #'#f
happens to be a valid contract expression. To maintain the distinction
even after converting to syntax, we wrap non-#f values in a box, which
`unit-body` subsequently unwraps. |#

(begin-for-syntax
  ;; (export-info identifier? identifier? (or/c syntax? #f)
  (struct export-info (ext-id loc-id ctc) #:transparent)
  ;; (var-info boolean? identifier? (or/c export-info? #f)
  (struct var-info (syntax? id [export-info #:mutable]) #:transparent))

(define-syntax (unit-body stx)
  (syntax-case stx ()
    ((_ err-stx
        ivars
        ([evar-int evar-ext eloc ectc] ...)
        body ...)
     (parameterize ((current-syntax-context #'err-stx))
       (let* ([expand-context (generate-expand-context)]
              [def-ctx (syntax-local-make-definition-context)]
              [stop-list
               (append
                (kernel-form-identifier-list)
                (syntax->list #'ivars))]
              [definition?
                (lambda (id)
                  (and (identifier? id)
                       (or (free-identifier=? id (quote-syntax define-values))
                           (free-identifier=? id (quote-syntax define-syntaxes)))))]
              [expanded-body
               (let expand-all ((defns&exprs (syntax->list #'(body ...))))
                 ;; Expand the body enough
                 (apply
                  append
                  (map
                   (lambda (defn-or-expr)
                     (let ([defn-or-expr
                             (local-expand
                              defn-or-expr
                              expand-context
                              stop-list
                              def-ctx)])
                       (define (track e)
                         (syntax-case defn-or-expr ()
                           [(id . _) (syntax-track-origin e defn-or-expr #'id)]))
                       (syntax-case defn-or-expr (begin define-values define-syntaxes)
                         [(begin . l)
                          (let ([l (parameterize ((current-syntax-context defn-or-expr))
                                     (checked-syntax->list #'l))])
                            (expand-all (map track l)))]
                         [(define-syntaxes (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (with-syntax ([rhs (local-transformer-expand
                                              #'rhs
                                              'expression
                                              null)])
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
                            (with-syntax ([(id ...) (map (lambda (id) (syntax-local-identifier-as-binding id def-ctx))
                                                    (syntax->list #'(id ...)))])
                              (list (track #'(define-syntaxes (id ...) rhs)))))]
                         [(define-values (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (begin
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f def-ctx)
                            (with-syntax ([(id ...) (map (lambda (id) (syntax-local-identifier-as-binding id def-ctx))
                                                         (syntax->list #'(id ...)))])
                              (list (track #'(define-values (id ...) rhs)))))]
                         [else (list defn-or-expr)])))
                   defns&exprs)))]
              [ends-in-defn?
               (syntax-case expanded-body ()
                 [(_ ... (x . _))
                  (and (identifier? #'x)
                       (member #'x
                               (list #'define-values #'define-syntaxes #'define-syntax)
                               free-identifier=?))]
                 [_ #f])]
              ;; Get all the defined names, sorting out variable definitions
              ;; from syntax definitions.
              [defined-names-table
                (let ((table (make-bound-id-table)))
                  (for-each
                   (lambda (defn-or-expr)
                     (syntax-case defn-or-expr ()
                       ((dv . rest)
                        (definition? #'dv)
                        (begin
                          (check-def-syntax defn-or-expr)
                          (syntax-case #'rest ()
                            [((id ...) expr)
                             (for-each 
                              (lambda (id)
                                (when (bound-id-table-ref table id #f)
                                  (raise-stx-err "variable defined twice" id))
                                (bound-id-table-set!
                                 table id 
                                 (var-info (free-identifier=? #'dv (quote-syntax define-syntaxes))
                                           id
                                           #f)))
                              (syntax->list #'(id ...)))]
                            [_ (void)])))
                       [_ (void)]))
                   expanded-body)
                  table)])
         
         ;; Mark exported names and
         ;; check that all exported names are defined (as var):
         (for ([int-id (in-list (syntax->list #'[evar-int ...]))]
               [ext-id (in-list (syntax->list #'[evar-ext ...]))]
               [loc-id (in-list (syntax->list #'[eloc ...]))]
               [ctc (in-list (syntax->list #'[ectc ...]))])
           (define v (bound-id-table-ref defined-names-table (localify int-id def-ctx) #f))
           (unless v
             (raise-stx-err (format "undefined export ~a" (syntax-e int-id))))
           (when (var-info-syntax? v)
             (raise-stx-err "cannot export syntax from a unit" int-id))
           (define ctc*
             ; see Note [Passing contracts to unit-body]
             (and (box? (syntax-e ctc))
                  (localify (unbox (syntax-e ctc)) def-ctx)))
           (set-var-info-export-info! v (export-info ext-id loc-id ctc*)))
         
         ;; Check that none of the imports are defined
         (for-each
          (lambda (i)
            (let ((defid (bound-id-table-ref defined-names-table i #f)))
              (when defid
                (raise-stx-err
                 "definition for imported identifier"
                 (var-info-id defid)))))
          (syntax->list (localify #'ivars def-ctx)))

         ;; Handles redirection of exported definitions and collects
         ;; positive-blaming `contract` expressions
         (define (process-defn-or-expr defn-or-expr)
           (syntax-case defn-or-expr (define-values)
             [(define-values (id ...) body)
              (let* ([ids (syntax->list #'(id ...))]
                     [tmps (generate-temporaries ids)]
                     [do-one
                      (λ (id tmp)
                        (let ([var-info (bound-id-table-ref defined-names-table id)])
                          (cond
                            [(var-info-export-info var-info)
                             =>
                             (λ (export-info)
                               (define ext-id (export-info-ext-id export-info))
                               (define loc-id (export-info-loc-id export-info))
                               (define ctc (export-info-ctc export-info))
                               (values
                                #`(begin
                                    #,(build-install-export!-expr loc-id tmp ext-id ctc)
                                    (define-syntax #,id
                                      (make-export-binding (quote-syntax #,tmp))))
                                (and ctc
                                     (quasisyntax/loc id
                                       ((unbox #,loc-id) #f)))))]
                            [else (values #`(define-syntax #,id
                                              (make-rename-transformer (quote-syntax #,tmp)))
                                          #f)])))])
                (define-values (defns-and-exprs ctc-exprs)
                  (for/lists [defns-and-exprs ctc-exprs]
                             ([id (in-list ids)]
                              [tmp (in-list tmps)])
                    (do-one id tmp)))
                (list (cons (syntax-track-origin
                             (quasisyntax/loc defn-or-expr
                               (define-values #,tmps
                                 #,(if (and (pair? ids)
                                            (null? (cdr ids))
                                            (not (syntax-property #'body 'inferred-name)))
                                       (syntax-property #'body 'inferred-name (car ids))
                                       #'body)))
                             defn-or-expr
                             (syntax-case defn-or-expr () [(d-v . _) #'d-v]))
                            defns-and-exprs)
                      (filter values ctc-exprs)))]
             [else (list (list defn-or-expr) '())]))

         (internal-definition-context-track
          def-ctx
          (if (null? expanded-body)
              #'(void)
              (with-syntax ([([(defn-or-expr ...) (ctc-expr ...)] ...)
                             (map process-defn-or-expr expanded-body)])
                (if ends-in-defn?
                    #'(let ()
                        defn-or-expr ... ...
                        ctc-expr ... ...
                        (void))
                    (with-syntax ([(defn-or-expr ... last-defn-or-expr) #'(defn-or-expr ... ...)])
                      #'(let ()
                          defn-or-expr ...
                          (begin0
                            last-defn-or-expr
                            ctc-expr ... ...))))))))))))

(define-syntax unit
  (syntax-parser
    [(_ . body)
     (define-values [u x y z] (build-unit #'body))
     u]))

;; -----------------------------------------------------------------------------
;; `unit-from-context`

#| Note [unit-from-context syntax properties]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expansion of `unit-from-context` is complicated, so we attach some
syntax properties to help Typed Racket identify which subexpressions
it needs to typecheck. We use two property keys for this purpose:

  * We attach a 'racket/unit:unit-from-context property to the entire
    unit expression. The value of this property is a gensym that
    uniquely identifies the unit.

  * We attach a 'racket/unit:unit-from-context:exported-expr property
    to each subexpression used as the unit’s export. These
    subexpressions are usually variable identifiers from the enclosing
    context, but in general they may be macro-introduced expressions
    or references to definitions introduced by `define-values-for-export`.

    The value of this property is a pair of a gensym and a natural
    number. The gensym is the same on stored in the 'racket/unit:unit-from-context
    property to make it easier to ensure the subexpression belongs to
    the unit. The natural number is the export’s index according to
    the ordering returned by `signature-members`.

The gensym identifying the unit expression is not generally crucial,
since normally a `unit-from-context` form does not contain any
subunits, but it is technically possible for such a subexpression to
be introduced via an identifier macro or a definition generated by
`define-values-for-export`. |#

;; build-unit-from-context : syntax-object -> 
;;                           (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a unit-from-context expression.  stx must be
;; such that it passes check-ufc-syntax.
;; The two additional values are the identifiers of the unit's import and export
;; signatures
(define-for-syntax build-unit-from-context
  (let ()
    (struct defn-info (index ext-id loc-id ctc dup-safe? post-def?) #:transparent)
    (syntax-parser
      #:context (current-syntax-context)
      [(out:tagged-export-spec)
       ;; see Note [unit-from-context syntax properties]
       (define unit-gensym (gensym 'unit-from-context))
       (define (add-unit-property unit-expr)
         (syntax-property unit-expr 'racket/unit:unit-from-context unit-gensym))

       ;; see Note [unit-from-context syntax properties]
       (define (add-export-property val-expr index)
         (syntax-property val-expr
                          'racket/unit:unit-from-context:exported-expr
                          (cons unit-gensym index)))
       
       ;; We can’t just expand to `unit` because that would evaluate imports
       ;; too early (see Note [Preserve import laziness] in "runtime.rkt"
       ;; for why), so we have to build the unit body directly.
       (define unit-expr
         (build-make-unit
          #:imports '()
          #:exports (list (attribute out.value))
          #:init-deps '()
          (λ ()
            (define-values [post-rhss ctcs] (build-post-val-defs+ctcs (attribute out.value)))

            (define post-id-table (make-bound-id-table))
            (for* ([post-ids (in-list (attribute out.post-def.id))]
                   [post-id (in-list post-ids)])
              (bound-id-table-set! post-id-table post-id #t))

            ;; see Note [Preserve import laziness] in "runtime.rkt"
            (define (safe-to-duplicate? int-id)
              (let ([val (syntax-local-value int-id (λ () #f))])
                (or (and (not (procedure? val))
                         (not (set!-transformer? val)))
                    (unit-variable-binding? val))))

            ;; We build the body in two steps. First, we analyze each signature
            ;; element to determine what we need to build.
            (define defns-table (make-bound-id-table))
            (define/syntax-parse
              ([defn-id ...]       ; int-ids we need to generate fresh definitions for
               [loc-id ...]        ; temp ids for the export boxes for the body definitions
               [ctcd-loc-id ...]   ; the subset of loc-ids that are contracted
               [export-thunk ...]) ; thunk expressions for the export vector
              (for/fold ([defn-ids '()]
                         [loc-ids '()]
                         [ctcd-loc-ids '()]
                         [export-thunks '()]
                         #:result (list (reverse defn-ids)
                                        (reverse loc-ids)
                                        (reverse ctcd-loc-ids)
                                        (reverse export-thunks)))
                        ([int-id (in-list (attribute out.var.int-id))]
                         [ext-id (in-list (attribute out.var.ext-id))]
                         [ctc (in-list ctcs)]
                         [index (in-naturals)])
                (define dup-safe? (safe-to-duplicate? int-id))
                (define post-def? (bound-id-table-ref post-id-table int-id #f))
                (cond
                  ;; If evaluating the expression multiple times is okay, and
                  ;; it isn’t bound by `define-values-for-export` and doesn’t need
                  ;; a contract, we can skip the temporary location altogether and
                  ;; just reference the binding directly in the export thunk.
                  [(and dup-safe? (not post-def?) (not ctc))
                   (values defn-ids
                           loc-ids
                           ctcd-loc-ids
                           (cons (quasisyntax/loc (current-syntax-context)
                                   (λ () #,(add-export-property int-id index)))
                                 export-thunks))]

                  ;; Otherwise, we need a temporary location. Record some
                  ;; information about the binding for later.
                  [else
                   (define loc-id (generate-temporary int-id))
                   (define export-thunk #`(λ () (check-not-unsafe-undefined (unbox #,loc-id) '#,ext-id)))
                   (bound-id-table-set! defns-table int-id (defn-info index ext-id loc-id ctc dup-safe? post-def?))
                   (values (if post-def? defn-ids (cons int-id defn-ids))
                           (cons loc-id loc-ids)
                           (if ctc (cons loc-id ctcd-loc-ids) ctcd-loc-ids)
                           (cons export-thunk export-thunks))])))

            ;; We use `local-intro` to distinguish body bindings from
            ;; the bindings they reference in the enclosing scope.
            (define local-intro (make-syntax-introducer))

            (quasisyntax/loc (current-syntax-context)
              (lambda ()
                (let ([loc-id (box unsafe-undefined)] ...)
                  (values
                   (with-loc+name #,(current-syntax-context)
                     (lambda (import-table)
                       ;; Generate definitions for any signature elements *not*
                       ;; defined by `define-values-for-export`.
                       #,@(for/list ([body-id (in-list (attribute defn-id))])
                            (define info (bound-id-table-ref defns-table body-id))
                            (define body-expr (add-export-property body-id (defn-info-index info)))
                            (define ctc (defn-info-ctc info))
                            (define local-body-id (local-intro body-id))
                            (cond
                              ;; If it’s safe to evaluate the value expression multiple
                              ;; times, then we need the export box to install a contract,
                              ;; but we don’t need a temporary location for the value itself,
                              ;; which improves laziness slightly (see Note [Preserve import
                              ;; laziness] in "runtime.rkt").
                              [(defn-info-dup-safe? info)
                               #`(begin
                                   (define-syntax #,local-body-id
                                     (make-export-binding (quote-syntax #,body-id)))
                                   #,(build-install-export!-expr
                                      (defn-info-loc-id info)
                                      body-expr
                                      (defn-info-ext-id info)
                                      (and ctc (local-intro ctc))))]
                              [else
                               (define tmp-id (generate-temporary body-id))
                               #`(begin
                                   (define #,tmp-id #,body-expr)
                                   (define-syntax #,local-body-id
                                     (make-export-binding (quote-syntax #,tmp-id)))
                                   #,(build-install-export!-expr
                                      (defn-info-loc-id info)
                                      local-body-id
                                      (defn-info-ext-id info)
                                      (and ctc (local-intro ctc))))]))

                       ;; Now generate definitions for each `define-values-for-export`
                       ;; in the signature. Note that these may not actually be exported,
                       ;; but they might be used in other definitions that are exported.
                       #,@(for/list ([post-ids (in-list (attribute out.post-def.id))]
                                     [post-rhs (in-list post-rhss)])
                            (define tmp-ids (generate-temporaries post-ids))
                            #`(begin
                                (define-values #,tmp-ids #,(local-intro post-rhs))
                                #,@(for/list ([post-id (in-list post-ids)]
                                              [tmp-id (in-list tmp-ids)])
                                     (define info (bound-id-table-ref defns-table post-id #f))
                                     (cond
                                       [info
                                        (define ctc (defn-info-ctc info))
                                        #`(begin
                                            (define-syntax #,(local-intro post-id)
                                              (make-export-binding (quote-syntax #,tmp-id)))
                                            #,(build-install-export!-expr
                                               (defn-info-loc-id info)
                                               (add-export-property tmp-id (defn-info-index info))
                                               (defn-info-ext-id info)
                                               (and ctc (local-intro ctc))))]
                                       [else
                                        #`(define-syntax #,(local-intro post-id)
                                            (make-rename-transformer (quote-syntax #,tmp-id)))]))))

                       ;; Report first-order violations for contracted bindings.
                       ((unbox ctcd-loc-id) #f) ...
                       (void)))
                   (unit-export ([out.key ...] (vector-immutable export-thunk ...))))))))))

       (values (syntax-parse-track-literals (add-unit-property unit-expr))
               '()
               (list (cons (attribute out.tag-sym) (attribute out.sig-id)))
               '())])))

(define-syntax unit-from-context
  (syntax-parser
    [(_ . body)
     (define-values [u x y z] (build-unit-from-context #'body))
     u]))

;; -----------------------------------------------------------------------------
;; `unit/new-import-export`

(begin-for-syntax
  ;; build-unit/new-import-export : syntax-object -> 
  ;;             (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit expression that changes the import and export signatures
  ;; of another.  stx must be such that it passes check-unit-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define (build-unit/new-import-export stx)
    (define/syntax-parse who (stx-car (current-syntax-context)))
    (syntax-parse stx
      #:context (current-syntax-context)
      #:literals [import export]
      [[(import in:tagged-import-spec ...)
        (export out:tagged-export-spec ...)
        deps:opt-init-depends
        {~describe "unit specification"
                   ([sub-out:tagged-import-spec ...]
                    sub-expr:expr
                    sub-in:tagged-export-spec ...)}]

       (define-values [base-unit-expr tagged-in-sigs tagged-out-sigs tagged-deps]
         (build-unit
          ; see Note [No generated exports for unit/new-import-export]
          #:suppress-generated-exports? #t
          #`[(import in ...)
             (export out ...)
             {~? {~@ . deps}}
             (with-loc+name #,(current-syntax-context)
               (define-values/invoke-unit sub-tmp
                 (import sub-in ...)
                 (export sub-out ...)
                 (values . results)))
             (apply values results)]))

       (define (check-all-bound import? in-idss out-idss)
         (define in-table (make-bound-id-table))
         (for* ([in-ids (in-list in-idss)]
                [in-id (in-list in-ids)])
           (bound-id-table-set! in-table in-id #t))
         (for* ([out-ids (in-list out-idss)]
                [out-id (in-list out-ids)])
           (unless (bound-id-table-ref in-table out-id #f)
             (wrong-syntax out-id "identifier ~a is not present in ~a"
                           (syntax-e out-id)
                           (if import? "new imports" "old exports")))))

       (check-all-bound #t (attribute in.var.int-id) (attribute sub-in.var.int-id))
       (check-all-bound #f (attribute sub-out.var.int-id) (attribute out.var.int-id))

       (define unit-expr
         (quasisyntax/loc (current-syntax-context)
           (let ([sub-tmp sub-expr])
             (check-unit sub-tmp 'who)
             (check-sigs
              sub-tmp
              (vector-immutable (cons 'sub-in.info.self-id (vector-immutable sub-in.key ...)) ...)
              (vector-immutable (cons 'sub-out.info.self-id (vector-immutable sub-out.key ...)) ...)
              'who)
             #,base-unit-expr)))

       (values (syntax-parse-track-literals (syntax-protect unit-expr))
               tagged-in-sigs
               tagged-out-sigs
               tagged-deps)])))

(define-syntax unit/new-import-export
  (syntax-parser
    [(_ . body)
     (define-values [u x y z] (build-unit/new-import-export #'body))
     u]))

;; -----------------------------------------------------------------------------
;; `compound-unit`

(begin-for-syntax
  (define-syntax-class tagged-link-id
    #:description "tagged link identifier"
    #:attributes [tag-id tag-sym link-id]
    #:commit
    #:literals [tag]
    (pattern (tag ~! tag-id:id link-id:id)
      #:attr tag-sym (syntax-e #'tag-id))
    (pattern link-id:id
      #:attr tag-id #f
      #:attr tag-sym #f))

  (define-syntax-class link-binding
    #:description "link binding"
    #:auto-nested-attributes
    #:commit
    #:datum-literals [:]
    (pattern [link-id:id : sig:tagged-signature-id]))

  (define-syntax-class linkage-decl
    #:description "linkage decl"
    #:auto-nested-attributes
    #:commit
    (pattern [(export:link-binding ...) unit-expr:expr import:tagged-link-id ...]))

  ;; In compound-unit, link-ids are bound by both the `import` clause and by
  ;; the exports section of a linkage-decl in a `link` clause. Information about
  ;; each link-id is stored in a link-id-binding record.
  (struct link-id-binding
    (order-index  ; The initialization order index of this link-id (0 for link-ids
                  ;   bound by imports and >0 for those bound by subunits).
     sig-id       ;\ The signature and siginfo of this link-id.
     siginfo      ;/
     access-expr) ; An expression that extracts the right signature vector
                  ;   from the appropriate unit table.
    #:transparent)

  ;; Returns #t if this link-id is bound by an import, #f if it is bound by a subunit.
  (define (link-id-binding-import? binding)
    (zero? (link-id-binding-order-index binding)))

  (define (link-id-binding->key-exprs binding tag)
    (siginfo->key-exprs (link-id-binding-siginfo binding) tag))

  ;; This utility function returns a list of natural numbers for use as a syntax
  ;; property needed to support units in Typed Racket.
  ;; Each number in the list is an index into a unit's list of imports signifying
  ;; that the import at that index is also an init-dependency of the unit.
  (define (build-init-depend-property init-depends imports)
    (define (sig=? s1 s2)
      (and (eq? (syntax-e (car s1)) (car s2))
           (free-identifier=? (cdr s1) (cdr s2))))
    (for/list ([import (in-list imports)]
               [index (in-naturals)]
               #:when (member import init-depends sig=?))
      index))

  ;; build-compound-unit : (->* [syntax?] [static-dep-info]
  ;;                            (values syntax?
  ;;                                    (listof identifier)
  ;;                                    (listof identifier)
  ;;                                    (listof identifier)))
  ;; Constructs the code for a compound-unit expression. The input syntax should
  ;; match the body of the compound-unit form (i.e. leaving off the initial
  ;; compound-unit identifier). The three additional return values are the
  ;; identifiers of the compound-unit's import and export signatures, plus
  ;; identifiers for initialization dependencies.
  (define (build-compound-unit stx [static-dep-info '()])
    (define/syntax-parse who:id (syntax-e (stx-car (current-syntax-context))))
    (define link-id-ctx (syntax-local-make-definition-context))
    (syntax-parse (internal-definition-context-add-scopes link-id-ctx stx)
      #:context (current-syntax-context)
      #:literals [import export link]
      [({~alt {~once (import in:link-binding ...)
                     #:too-few "missing import clause"
                     #:too-many "multiple import clauses"}
              {~once (export out:tagged-link-id ...)
                     #:too-few "missing export clause"
                     #:too-many "multiple export clauses"}
              {~once (link sub:linkage-decl ...)
                     #:too-few "missing link clause"
                     #:too-many "multiple link clauses"}}
        ...)
       #:fail-when (check-duplicate-identifier (append (attribute in.link-id)
                                                       (append* (attribute sub.export.link-id))))
                   "duplicate linking identifier definition"

       ;; Step 0: Bind some temporaries for use in the generated code.
       (define/syntax-parse deps-id (generate-temporary 'deps))
       (define/syntax-parse import-table-id (generate-temporary 'import-table))
       (define/syntax-parse [sub-unit-id ...] (generate-temporaries (attribute sub.unit-expr)))
       (define/syntax-parse [sub-export-table-id ...] (generate-temporaries (attribute sub.unit-expr)))

       ;; Step 1: Bind each `link-id` to a `link-id-binding` record so we can query it.
       (for ([order-index (in-naturals)]
             [link-ids (in-list (cons (attribute in.link-id)
                                      (attribute sub.export.link-id)))]
             [sig-ids (in-list (cons (attribute in.sig.sig-id)
                                     (attribute sub.export.sig.sig-id)))]
             [siginfos (in-list (cons (attribute in.sig.info)
                                      (attribute sub.export.sig.info)))]
             [key-exprs (in-list (cons (attribute in.sig.self-key)
                                       (attribute sub.export.sig.self-key)))]
             [table-id (in-list (cons (attribute import-table-id)
                                      (attribute sub-export-table-id)))])
         (for ([link-id (in-list link-ids)]
               [sig-id (in-list sig-ids)]
               [siginfo (in-list siginfos)]
               [key-expr (in-list key-exprs)])
           (syntax-local-bind-syntaxes
            (list link-id)
            #`(quote #,(link-id-binding order-index
                                        sig-id
                                        siginfo
                                        #`(hash-ref #,table-id #,key-expr)))
            link-id-ctx)))

       (define (lookup-link link-id)
         (define v (syntax-local-value link-id (λ () #f) link-id-ctx))
         (unless (link-id-binding? v)
           (wrong-syntax link-id "unknown linking identifier"))
         v)

       ;; Step 2: Do some simple compile-time checks.
       (for ([out-link-id (in-list (attribute out.link-id))])
         (define binding (lookup-link out-link-id))
         (when (link-id-binding-import? (lookup-link out-link-id))
           (wrong-syntax out-link-id "cannot directly export an import")))

       (define (do-check-duplicate-subs source-stxs link-bindings tag-syms)
         (check-duplicate-subs
          (for/list ([link-binding (in-list link-bindings)]
                     [tag-sym (in-list tag-syms)])
            (cons tag-sym (link-id-binding-siginfo link-binding)))
          source-stxs))

       (do-check-duplicate-subs (attribute out)
                                (map lookup-link (attribute out.link-id))
                                (attribute out.tag-sym))

       ;; Step 3: Resolve import/export linkages.
       (define (get-import-output-linkage link-ids tag-syms)
         (for/list ([link-id (in-list link-ids)]
                    [tag-sym (in-list tag-syms)])
           (define binding (lookup-link link-id))
           (list* (car (siginfo-names (link-id-binding-siginfo binding)))
                  (link-id-binding-access-expr binding)
                  (link-id-binding->key-exprs binding tag-sym))))

       (define/syntax-parse ([in-name-id _ in-key-expr ...] ...)
         (get-import-output-linkage (attribute in.link-id) (attribute in.sig.tag-sym)))
       (define/syntax-parse ([out-name-id out-access-expr out-key-expr ...] ...)
         (get-import-output-linkage (attribute out.link-id) (attribute out.tag-sym)))

       ;; Step 4: Resolve sub-unit imports linkages. The `check-sub-expr`s are
       ;; evaluated when the compound-unit form is evaluated and check that the
       ;; linkages are valid, and the `sub-import-table-expr`s build the import
       ;; tables passed to the unit when it is invoked.
       (define/syntax-parse [(check-sub-expr . sub-import-table-expr) ...]
         (for/list ([order-index (in-naturals 1)] ; imports have index 0, so start at 1
                    [unit-expr (in-list (attribute sub.unit-expr))]
                    [unit-id (in-list (attribute sub-unit-id))]
                    [in-stxs (in-list (attribute sub.import))]
                    [in-link-ids (in-list (attribute sub.import.link-id))]
                    [in-tags (in-list (attribute sub.import.tag-sym))]
                    [out-link-ids (in-list (attribute sub.export.link-id))]
                    [out-tags (in-list (attribute sub.export.sig.tag-sym))])
           (define in-bindings (map lookup-link in-link-ids))
           (define out-bindings (map lookup-link out-link-ids))
           (do-check-duplicate-subs in-stxs in-bindings in-tags)

           (define (get-names+keys bindings tags)
             (map (λ (binding tag)
                    (cons (car (siginfo-names (link-id-binding-siginfo binding)))
                          (link-id-binding->key-exprs binding tag)))
                  bindings
                  tags))
           (define/syntax-parse ([in-sig-name in-key-expr ...] ...) (get-names+keys in-bindings in-tags))
           (define/syntax-parse ([out-sig-name out-key-expr ...] ...) (get-names+keys out-bindings out-tags))

           ;; Analyze this sub-unit’s position in the unit initialization order
           ;; to construct the appropriate init-dep checks.
           (define/syntax-parse ([import-dep-expr ...]
                                 [(forward-dep-key-expr . forward-dep-names-expr) ...])
             (for/fold ([import-deps '()]
                        [forward-deps '()]
                        #:result (list import-deps forward-deps))
                       ([in-link-id (in-list in-link-ids)]
                        [in-binding (in-list in-bindings)]
                        [in-tag (in-list in-tags)])
               (cond
                 [(link-id-binding-import? in-binding)
                  (values (append (link-id-binding->key-exprs in-binding in-tag)
                                  import-deps)
                          forward-deps)]
                 [(>= (link-id-binding-order-index in-binding) order-index)
                  (values import-deps
                          (for/fold ([forward-deps forward-deps])
                                    ([dep-key-expr (in-list (link-id-binding->key-exprs in-binding in-tag))]
                                     [sig-name (in-list (siginfo-names (link-id-binding-siginfo in-binding)))])
                            (cons (cons dep-key-expr #`(cons '#,sig-name '#,in-link-id))
                                  forward-deps)))]
                 [else
                  (values import-deps forward-deps)])))

           (cons
            #`(begin
                ;; check that the unit expression is actually a unit
                #,(quasisyntax/loc unit-expr
                    (check-unit #,unit-id 'who))
                ;; check that the unit imports/exports the right signatures
                #,(quasisyntax/loc unit-expr
                    (check-sigs #,unit-id
                                (vector-immutable (cons 'in-sig-name
                                                        (vector-immutable in-key-expr ...))
                                                  ...)
                                (vector-immutable (cons 'out-sig-name
                                                        (vector-immutable out-key-expr ...))
                                                  ...)
                                'who))
                ;; check that the unit’s init-depends constraints are satisfied
                #,(quasisyntax/loc unit-expr
                    (check-deps (hash {~@ forward-dep-key-expr forward-dep-names-expr} ...)
                                #,unit-id
                                'who))
                ;; record any of the unit’s init-depends on imports
                (let ([import-deps (hash {~@ import-dep-expr #t} ...)])
                  (for-each (lambda (dep)
                              (when (hash-has-key? import-deps dep)
                                (set! deps-id (cons dep deps-id))))
                            (unit-deps #,unit-id))))

            (let ()
              (define/syntax-parse (([key-expr . access-expr] ...) ...)
                (for/list ([binding (in-list in-bindings)]
                           [key-exprs (in-list (attribute in-key-expr))])
                  (define access-expr (link-id-binding-access-expr binding))
                  (map (λ (key-expr) (cons key-expr access-expr)) key-exprs)))
              #`(hash {~@ key-expr access-expr} ... ...)))))

       ;; Step 6: Assemble the generated expression.
       (define compound-unit-expr
         (quasisyntax/loc this-syntax
           (let ([deps-id '()]
                 [sub-unit-id sub.unit-expr] ...)
             check-sub-expr ...
             (make-unit
              '#,(syntax-local-infer-name (current-syntax-context))
              (vector-immutable (cons 'in-name-id (vector-immutable in-key-expr ...)) ...)
              (vector-immutable (cons 'out-name-id (vector-immutable out-key-expr ...)) ...)
              (remove-duplicates deps-id)
              (lambda ()
                (let-values ([(sub-unit-id sub-export-table-id) ((unit-go sub-unit-id))] ...)
                  (values (lambda (import-table-id)
                            (void) ; just in case there are no sub-units
                            (sub-unit-id sub-import-table-expr) ...)
                          (unit-export ([out-key-expr ...] out-access-expr) ...))))))))

       ;; Step 7: Build static information.
       ;; TODO: These values come in a strange format. Really, it would make
       ;; sense for them to match the results of `unit-static-signatures` from
       ;; racket/unit-exptime, but instead they have an extra layer of syntax
       ;; wrapping around the `car` of each pair. It would be nice to fix this.
       (define static-imports (map syntax-e (syntax->list #'(({~? in.sig.tag-id #f} . in.sig.sig-id) ...))))
       (define/syntax-parse [out-sig-id ...] (for/list ([out-link-id (in-list (attribute out.link-id))])
                                               (link-id-binding-sig-id (lookup-link out-link-id))))
       (define static-exports (map syntax-e (syntax->list #'(({~? out.tag-id #f} . out-sig-id) ...))))

       ;; We’re done!
       (values (syntax-parse-track-literals
                (syntax-protect
                 (syntax-property
                  compound-unit-expr
                  'unit:inferred-init-depends
                  (build-init-depend-property static-dep-info static-imports))))
               static-imports
               static-exports
               static-dep-info)])))

(define-syntax compound-unit
  (syntax-parser
    [(_ . body)
     (define-values [u x y z] (build-compound-unit #'body))
     u]))

;; -----------------------------------------------------------------------------
;; `invoke-unit`

(define (invoke-unit/core unit)
  (check-unit unit 'invoke-unit)
  (check-no-imports unit 'invoke-unit)
  (let-values ([(f exports) ((unit-go unit))])
    (f #f)))

(begin-for-syntax
  (define-syntax-class invoke-results-clause
    #:description "values clause"
    #:attributes [formals {id 1}]
    #:commit
    #:literals [values]
    (pattern (values . {~and formals (x:id ... . {~or* rest:id ()})})
      #:with [id ...] #'[x ... {~? rest}])))

(define-syntax define-values/invoke-unit/core
  (syntax-parser
    #:track-literals
    #:literals [export]
    [(_ unit-expr:expr
        (export out:tagged-import-spec ...)
        {~optional results:invoke-results-clause})
     (let ([dup-id (check-duplicate-identifier (append-map signature-ie-int-names (attribute out.value)))])
       (when dup-id
         (raise-stx-err (format "duplicate binding for ~.s" (syntax-e dup-id)))))

     (define/syntax-parse [out-vec-id ...] (generate-temporaries (attribute out)))
     (define/syntax-parse ([rename-bind [stx-bind ...] [val-bind ...]] ...)
       (map (build-val+macro-defs values) (attribute out.value)))

     (define/syntax-parse ([out-expr ...] ...)
       (for/list ([vec-id (in-list (attribute out-vec-id))]
                  [ctcs (in-list (attribute out.ctc))])
         (for/list ([(ctc i) (in-indexed (in-list ctcs))])
           (build-unbox-import-expr (current-syntax-context)
                                    #`(vector-ref #,vec-id #,i)
                                    ctc))))
     #`(begin
         (define-values [{~? {~@ results.id ...}} out.var.int-id ... ...]
           #,(quasisyntax/loc (current-syntax-context)
               (let ([unit-tmp unit-expr])
                 (check-unit unit-tmp 'define-values/invoke-unit)
                 (check-sigs unit-tmp
                             (vector-immutable)
                             (vector-immutable (cons 'out.info.self-id
                                                     (vector-immutable out.key ...)) ...)
                             'define-values/invoke-unit)
                 (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                   (let ([out-vec-id (hash-ref export-table out.self-key)] ...)
                     #,(if (attribute results)
                           #`(call-with-values
                              (λ ()
                                #,(syntax/loc (current-syntax-context)
                                    (unit-fn #f)))
                              #,(syntax/loc (current-syntax-context)
                                  (λ results.formals
                                    (values results.id ... out-expr ... ...))))
                           #'(begin
                               (unit-fn #f)
                               (values out-expr ... ...))))))))
         (define-syntaxes . rename-bind) ...
         (define-syntaxes . stx-bind) ... ...
         (define-values . val-bind) ... ...)]))

(define-for-syntax (temp-id-with-tags id i)
  (let loop ([i i] [at #f])
    (syntax-case i (tag bind-at)
      [(tag t sig)
       (list id #`(tag t #,id) (let ([l #'(tag t sig)])
                                 (if at
                                     #`(bind-at #,at #,l)
                                     l)))]
      [(bind-at u i)
       (loop #'i #'u)]
      [_else
       (list id id (if at
                       #`(bind-at #,at #,i)
                       i))])))

(define-syntax invoke-unit
  (syntax-parser
    #:track-literals
    #:literals [import]
    [(_ unit:expr)
     (syntax/loc this-syntax
       (invoke-unit/core unit))]
    [(_ unit:expr (import in:tagged-import-spec ...))
     (define/syntax-parse [u ...] (generate-temporaries (attribute in)))
     (define/syntax-parse ([U Ul isig] ...) (map temp-id-with-tags
                                                 (generate-temporaries (attribute in))
                                                 (attribute in)))
     (quasisyntax/loc this-syntax
       (let ()
         (define u (with-loc+name #,this-syntax
                     (unit-from-context in)))
         ...
         (with-loc #,this-syntax
           (invoke-unit/core
            (with-loc+name #,this-syntax
              (compound-unit
               (import) (export)
               (link [((U : in.sig-id)) u] ... [() unit Ul ...])))))))]))

(define-syntax define-values/invoke-unit
  (syntax-parser
    #:track-literals
    #:literals [import export]
    [(_ {~describe "unit expression" u:expr}
        {~describe "import clause" (import i:tagged-export-spec ...)}
        {~describe "export clause" (export e:tagged-import-spec ...)}
        {~optional results:invoke-results-clause})
     (if (empty? (attribute i))
         (quasisyntax/loc this-syntax
           (define-values/invoke-unit/core u
             (export e ...)
             {~? results}))
         (with-syntax ((((EU EUl e) ...) (map temp-id-with-tags
                                              (generate-temporaries #'(e ...))
                                              (syntax->list #'(e ...))))
                       (((IU IUl i) ...) (map temp-id-with-tags
                                              (generate-temporaries #'(i ...))
                                              (syntax->list #'(i ...))))
                       ((iu ...) (generate-temporaries #'(i ...))))
           (quasisyntax/loc this-syntax
             (begin
               (define iu (with-loc+name #,this-syntax
                            (unit-from-context i)))
               ...
               (with-loc #,this-syntax
                 (define-values/invoke-unit/core
                   (with-loc+name #,this-syntax
                     (compound-unit
                      (import) (export EUl ...)
                      (link [((IU : i.tagged-sig-id)) iu] ... [((EU : e.tagged-sig-id) ...) u IUl ...])))
                   (export e ...)
                   {~? results}))))))]))
