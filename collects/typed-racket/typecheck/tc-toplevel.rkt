#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         syntax/kerncase
         unstable/list racket/syntax syntax/parse
         mzlib/etc racket/list
         racket/match
         "signatures.rkt"
         "tc-structs.rkt"
         "typechecker.rkt"
         ;; to appease syntax-parse
         "internal-forms.rkt"
         (rep type-rep free-variance)
         (types utils abbrev type-table)
         (private parse-type type-annotation type-contract)
         (env global-env init-envs type-name-env type-alias-env lexical-env env-req mvar-env)
         syntax/id-table
         (utils tc-utils mutated-vars)
         "provide-handling.rkt"
         "def-binding.rkt"
         (prefix-in c: racket/contract)
         racket/dict
         syntax/location
         (for-template
          "internal-forms.rkt"
          syntax/location
          mzlib/contract
          racket/base
          "../env/env-req.rkt"))

(c:provide/contract
 [type-check (syntax? . c:-> . (values syntax? syntax?))]
 [tc-module (syntax? . c:-> . (values syntax? syntax?))]
 [tc-toplevel-form (syntax? . c:-> . (values #f c:any/c))])

(define unann-defs (make-free-id-table))

(define-splicing-syntax-class dtsi-fields
 #:attributes (mutable type-only maker)
 (pattern
  (~seq
    (~or (~optional (~and #:mutable (~bind (mutable #t))))
         (~optional (~and #:type-only (~bind (type-only #t))))
         (~optional (~seq #:maker maker))) ...)))

(define-syntax-class struct-name
 (pattern nm:id)
 (pattern (nm:id parent:id)))


(define-syntax-class define-typed-struct
  #:attributes (name mutable type-only maker nm (tvars 1) (fld 1) (ty 1))
  (pattern ((~optional (tvars:id ...) #:defaults (((tvars 1) null)))
            nm:struct-name ([fld:id : ty:expr] ...) fields:dtsi-fields)
           #:attr name #'nm.nm
           #:attr mutable (attribute fields.mutable)
           #:attr type-only (attribute fields.type-only)
           #:attr maker (attribute fields.maker)))

(define (parse-define-struct-internal form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literals (values define-typed-struct-internal
                  define-typed-struct/exec-internal quote-syntax #%plain-app)

      ;; define-typed-struct
      [(define-values () (begin (quote-syntax (define-typed-struct-internal ~! . dts:define-typed-struct)) (#%plain-app values)))
       (tc/struct (attribute dts.tvars) #'dts.nm (syntax->list #'(dts.fld ...)) (syntax->list #'(dts.ty ...))
                  #:mutable (attribute dts.mutable)
                  #:maker (attribute dts.maker)
                  #:type-only (attribute dts.type-only))]

      ;; executable structs - this is a big hack
      [(define-values () (begin (quote-syntax (define-typed-struct/exec-internal ~! nm ([fld : ty] ...) proc-ty)) (#%plain-app values)))
       (tc/struct null #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #:proc-ty #'proc-ty)])))

(define (add-constant-variance! form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literals (values define-typed-struct-internal quote-syntax #%plain-app)
      ;; define-typed-struct
      [(define-values () (begin (quote-syntax (define-typed-struct-internal ~! . dts:define-typed-struct)) (#%plain-app values)))
       ;; TODO make constant
       (unless (null? (attribute dts.tvars))
         (register-type-variance! #'dts.name (map (lambda (_) Covariant) (attribute dts.tvars))))]
      [(define-values () (begin (quote-syntax (define-typed-struct/exec-internal ~! nm ([fld : ty] ...) proc-ty)) (#%plain-app values)))
       ;; Not polymorphic
       (void)])))






(define (tc-toplevel/pass1 form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literals (values define-type-alias-internal define-typed-struct-internal define-type-internal
                         define-typed-struct/exec-internal :-internal assert-predicate-internal
                         require/typed-internal declare-refinement-internal
                         define-values quote-syntax #%plain-app begin define-syntaxes)
      ;#:literal-sets (kernel-literals)

      ;; forms that are handled in other ways
      [stx
       #:when (or (syntax-property form 'typechecker:ignore)
                  (syntax-property form 'typechecker:ignore-some))
       (list)]

      [((~literal module) n:id spec ((~literal #%plain-module-begin) body ...))
       (list)]
       ;; module* is not expanded, so it doesn't have a `#%plain-module-begin`
      [((~literal module*) n:id spec body ...)
       (list)]

      ;; type aliases have already been handled by an earlier pass
      [(define-values () (begin (quote-syntax (define-type-alias-internal nm ty)) (#%plain-app values)))
       (list)]

      ;; declare-refinement
      ;; FIXME - this sucks and should die
      [(define-values () (begin (quote-syntax (declare-refinement-internal pred)) (#%plain-app values)))
       (match (lookup-type/lexical #'pred)
              [(and t (Function: (list (arr: (list dom) (Values: (list (Result: rng _ _))) #f #f '()))))
               (let ([new-t (make-pred-ty (list dom)
                                          rng
                                          (make-Refinement dom #'pred (syntax-local-certifier)))])
                 (register-type #'pred new-t))
               (list)]
              [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]

      ;; require/typed
      [(define-values () (begin (quote-syntax (require/typed-internal nm ty)) (#%plain-app values)))
       (let ([t (parse-type #'ty)])
         (register-type #'nm t)
         (list (make-def-binding #'nm t)))]

      [(define-values () (begin (quote-syntax (require/typed-internal nm ty #:struct-maker parent)) (#%plain-app values)))
       (let* ([t (parse-type #'ty)]
              [flds (map fld-t (Struct-flds (lookup-type-name (Name-id t))))]
              [mk-ty (flds #f . ->* . t)])
         (register-type #'nm mk-ty)
         (list (make-def-binding #'nm mk-ty)))]

      ;; define-typed-struct (handled earlier)
      [(define-values () (begin (quote-syntax (define-typed-struct-internal . _)) (#%plain-app values)))
       (list)]

      ;; executable structs (handled earlier)
      [(define-values () (begin (quote-syntax (define-typed-struct/exec-internal . _)) (#%plain-app values)))
       (list)]

      ;; predicate assertion - needed for define-type b/c or doesn't work
      [(define-values () (begin (quote-syntax (assert-predicate-internal ty pred)) (#%plain-app values)))
       (register-type #'pred (make-pred-ty (parse-type #'ty)))]

      ;; top-level type annotation
      [(define-values () (begin (quote-syntax (:-internal id:identifier ty)) (#%plain-app values)))
       (register-type/undefined #'id (parse-type #'ty))]


      ;; values definitions
      [(define-values (var ...) expr)
       (let* ([vars (syntax->list #'(var ...))])
         (cond
           ;; if all the variables have types, we stick them into the environment
           [(andmap (lambda (s) (syntax-property s 'type-label)) vars)
            (let ([ts (map (λ (x) (get-type x #:infer #f)) vars)])
              (for-each register-type-if-undefined vars ts)
              (map make-def-binding vars ts))]
           ;; if this already had an annotation, we just construct the binding reps
           [(andmap (lambda (s) (lookup-type s (lambda () #f))) vars)
            (for-each finish-register-type vars)
            (map (lambda (s) (make-def-binding s (lookup-type s))) vars)]
           ;; special case to infer types for top level defines
           [else
            (match (get-type/infer vars #'expr tc-expr tc-expr/check)
              [(tc-results: ts)
               (for/list ([i (in-list vars)] [t (in-list ts)])
                 (register-type i t)
                 (free-id-table-set! unann-defs i #t)
                 (make-def-binding i t))])]))]

      ;; to handle the top-level, we have to recur into begins
      [(begin . rest)
       (apply append (filter list? (map tc-toplevel/pass1 (syntax->list #'rest))))]

      ;; define-syntaxes just get noted
      [(define-syntaxes (var:id ...) . rest)
       (map make-def-stx-binding (syntax->list #'(var ...)))]

      ;; otherwise, do nothing in this pass
      ;; handles expressions, provides, requires, etc and whatnot
      [_ (list)])))





;; typecheck the expressions of a module-top-level form
;; no side-effects
;; syntax -> void
(define (tc-toplevel/pass2 form)
  (parameterize ([current-orig-stx form])
    (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal
                                   require/typed-internal values module module*)
      ;; these forms we have been instructed to ignore
      [stx
       (syntax-property form 'typechecker:ignore)
       (void)]

      ;; this is a form that we mostly ignore, but we check some interior parts
      [stx
       (syntax-property form 'typechecker:ignore-some)
       (check-subforms/ignore form)]

      ;; these forms should always be ignored
      [(#%require . _) (void)]
      [(#%provide . _) (void)]
      [(define-syntaxes . _) (void)]
      [(begin-for-syntax . _) (void)]

      ;; FIXME - we no longer need these special cases
      ;; these forms are handled in pass1
      [(define-values () (begin (quote-syntax (require/typed-internal . rest)) (#%plain-app values)))
       (void)]
      [(define-values () (begin (quote-syntax (define-type-alias-internal . rest)) (#%plain-app values)))
       (void)]
      [(define-values () (begin (quote-syntax (define-typed-struct-internal . rest)) (#%plain-app values)))
       (void)]

      ;; submodules take care of themselves:
      [(module n spec (#%plain-module-begin body ...))
       (void)]
      ;; module* is not expanded, so it doesn't have a `#%plain-module-begin`
      [(module* n spec body ...)
       (void)]

      ;; definitions just need to typecheck their bodies
      [(define-values (var ...) expr)
       (let* ([vars (syntax->list #'(var ...))]
              [ts (map lookup-type vars)])
         (unless (for/and ([v (syntax->list #'(var ...))])
                   (free-id-table-ref unann-defs v (lambda _ #f)))
           (tc-expr/check #'expr (ret ts)))
         (void))]

      ;; to handle the top-level, we have to recur into begins
      [(begin) (void)]
      [(begin . rest)
       (let loop ([l (syntax->list #'rest)])
         (if (null? (cdr l))
             (tc-toplevel/pass2 (car l))
             (begin (tc-toplevel/pass2 (car l))
                    (loop (cdr l)))))]

      ;; otherwise, the form was just an expression
      [_ (tc-expr form)])))



;; new implementation of type-check
(define-syntax-rule (internal-syntax-pred nm)
  (lambda (form)
    (kernel-syntax-case* form #f
      (nm values)
      [(define-values () (begin (quote-syntax (nm . rest)) (#%plain-app values)))
       #t]
      [_ #f])))

(define (parse-def x)
  (kernel-syntax-case x #f
    [(define-values (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define (parse-syntax-def x)
  (kernel-syntax-case x #f
    [(define-syntaxes (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))


(define (add-type-name! names)
  (for-each register-type-name names))

(define (parse-type-alias form)
  (kernel-syntax-case* form #f
    (define-type-alias-internal values)
    [(define-values () (begin (quote-syntax (define-type-alias-internal nm ty)) (#%plain-app values)))
     (values #'nm #'ty)]
    [_ (int-err "not define-type-alias")]))

;; actually do the work on a module
;; produces prelude and post-lude syntax objects
;; syntax-list -> (values syntax syntax)
(define (type-check forms0)
  (define forms (syntax->list forms0))
  (define-values (type-aliases struct-defs stx-defs0 val-defs0 provs reqs)
    (filter-multiple
     forms
     (internal-syntax-pred define-type-alias-internal)
     (lambda (e) (or ((internal-syntax-pred define-typed-struct-internal) e)
                     ((internal-syntax-pred define-typed-struct/exec-internal) e)))
     parse-syntax-def
     parse-def
     provide?
     define/fixup-contract?))
  (do-time "Form splitting done")
  ;(printf "before parsing type aliases~n")
  (for-each (compose register-type-alias parse-type-alias) type-aliases)
  ;; Add the struct names to the type table, but not with a type
  ;(printf "before adding type names~n")
  (for-each (compose add-type-name! names-of-struct) struct-defs)
  (for-each add-constant-variance! struct-defs)
  ;(printf "after adding type names~n")
  ;; resolve all the type aliases, and error if there are cycles
  (resolve-type-aliases parse-type)
  ;; Parse and register the structure types
  (define parsed-structs
    (for/list ((def struct-defs))
      (define parsed (parse-define-struct-internal def))
      (register-parsed-struct-sty! parsed)
      parsed))

  (refine-struct-variance! parsed-structs)

  ;; register the bindings of the structs
  (define struct-bindings (map register-parsed-struct-bindings! parsed-structs))
  ;(printf "after resolving type aliases~n")
  ;(displayln "Starting pass1")
  ;; do pass 1, and collect the defintions
  (define defs (apply append 
                      (append
                       struct-bindings
                       (filter list? (map tc-toplevel/pass1 forms)))))
  ;(displayln "Finished pass1")
  ;; separate the definitions into structures we'll handle for provides
  (define def-tbl
    (for/fold ([h (make-immutable-free-id-table)])
      ([def (in-list defs)])
      (dict-set h (binding-name def) def)))
  ;; typecheck the expressions and the rhss of defintions
  ;(displayln "Starting pass2")
  (for-each tc-toplevel/pass2 forms)
  ;(displayln "Finished pass2")
  ;; check that declarations correspond to definitions
  (check-all-registered-types)
  ;; report delayed errors
  (report-all-errors)
  (define syntax-provide? #f)
  (define provide-tbl
    (for/fold ([h (make-immutable-free-id-table)]) ([p (in-list provs)])
      (define-syntax-class unknown-provide-form
        (pattern
         (~and name
               (~or (~datum protect) (~datum for-syntax) (~datum for-label) (~datum for-meta)
                    (~datum struct) (~datum all-from) (~datum all-from-except)
                    (~datum all-defined) (~datum all-defined-except)
                    (~datum prefix-all-defined) (~datum prefix-all-defined-except)
                    (~datum expand)))))
      (syntax-parse p #:literals (#%provide)
        [(#%provide form ...)
         (for/fold ([h h]) ([f (syntax->list #'(form ...))])
           (parameterize ([current-orig-stx f])
             (syntax-parse f
               [i:id
                (when (def-stx-binding? (dict-ref def-tbl #'i #f))
                  (set! syntax-provide? #t))
                (dict-update h #'i (lambda (tail) (cons #'i tail)) '())]
               [((~datum rename) in out)
                (when (def-stx-binding? (dict-ref def-tbl #'in #f))
                  (set! syntax-provide? #t))
                (dict-update h #'in (lambda (tail) (cons #'out tail)) '())]
               [(name:unknown-provide-form . _)
                (tc-error "provide: ~a not supported by Typed Racket" (syntax-e #'name.name))]
               [_ (int-err "unknown provide form")])))]
        [_ (int-err "non-provide form! ~a" (syntax->datum p))])))
  ;; compute the new provides
  (define-values (new-stx/pre new-stx/post) 
    (with-syntax*
     ([the-variable-reference (generate-temporary #'blame)])
     (define-values (code aliasess)
       (generate-prov def-tbl provide-tbl #'the-variable-reference))
     (define aliases (apply append aliasess))
     (define/with-syntax (new-provs ...) code)
     (values
      #`(begin
          (begin-for-syntax
            (module* #%type-decl #f
              (require typed-racket/types/numeric-tower typed-racket/env/type-name-env
                       typed-racket/env/global-env typed-racket/env/type-alias-env
                       typed-racket/types/type-table)
              #,(env-init-code syntax-provide? provide-tbl def-tbl)
              #,(talias-env-init-code)
              #,(tname-env-init-code)
              #,(tvariance-env-init-code)
              #,(mvar-env-init-code mvar-env)
              #,(make-struct-table-code)
              #,@(for/list ([a (in-list aliases)])
                   (match a
                     [(list from to)
                      #`(add-alias (quote-syntax #,from) (quote-syntax #,to))]))))
          (begin-for-syntax (add-mod! (quote-module-path))))
      #`(begin
          #,(if (null? (syntax-e #'(new-provs ...)))
                #'(begin)
                #'(define the-variable-reference (quote-module-name)))
          new-provs ...))))
  (do-time "finished provide generation")
  (values new-stx/pre new-stx/post))

;; typecheck a whole module
;; syntax -> (values syntax syntax)
(define (tc-module stx)
  (syntax-parse stx
    [(pmb . forms) (type-check #'forms)]))

;; typecheck a top-level form
;; used only from #%top-interaction
;; syntax -> (values #f (or/c void? tc-results?))
(define (tc-toplevel-form form)
  (tc-toplevel/pass1 form)
  (begin0 (values #f (tc-toplevel/pass2 form))
          (report-all-errors)))

