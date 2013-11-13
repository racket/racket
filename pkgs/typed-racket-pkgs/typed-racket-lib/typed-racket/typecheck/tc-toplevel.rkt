#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/syntax syntax/parse syntax/stx syntax/id-table
         racket/list unstable/list racket/dict racket/match unstable/sequence
         (prefix-in c: (contract-req))
         (rep type-rep free-variance)
         (types utils abbrev type-table struct-table)
         (private parse-type type-annotation type-contract syntax-properties)
         (env global-env init-envs type-name-env type-alias-env
              lexical-env env-req mvar-env scoped-tvar-env)
         (utils tc-utils)
         (typecheck provide-handling def-binding tc-structs
                    typechecker internal-forms)

         syntax/location

         (for-template
          syntax/location
          racket/base
          (env env-req)))

(provide/cond-contract
 [type-check (syntax? . c:-> . (values syntax? syntax?))]
 [tc-module (syntax? . c:-> . (values syntax? syntax?))]
 [tc-toplevel-form (syntax? . c:-> . (values #f c:any/c))])

(define unann-defs (make-free-id-table))

(define (parse-typed-struct form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      [t:typed-struct
       (tc/struct (attribute t.tvars) #'t.nm (syntax->list #'(t.fields ...)) (syntax->list #'(t.types ...))
                  #:mutable (attribute t.mutable)
                  #:maker (attribute t.maker)
                  #:type-only (attribute t.type-only))]
      [t:typed-struct/exec
       (tc/struct null #'t.nm (syntax->list #'(t.fields ...)) (syntax->list #'(t.types ...))
                  #:proc-ty #'t.proc-type)])))

(define (type-vars-of-struct form)
  (syntax-parse form
    [t:typed-struct (attribute t.tvars)]
    [t:typed-struct/exec null]))

(define (add-constant-variance! name vars)
  (unless (null? vars)
    (register-type-variance! name (map (lambda (_) Constant) vars))))




;; syntax? -> (listof def-binding?)
(define (tc-toplevel/pass1 form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literals (values define-values #%plain-app begin define-syntaxes)
      ;#:literal-sets (kernel-literals)

      ;; forms that are handled in other ways
      [stx
       #:when (or (ignore-property form) (ignore-some-property form))
       (list)]

      [((~literal module) n:id spec ((~literal #%plain-module-begin) body ...))
       (list)]
       ;; module* is not expanded, so it doesn't have a `#%plain-module-begin`
      [((~literal module*) n:id spec body ...)
       (list)]

      ;; type aliases have already been handled by an earlier pass
      [_:type-alias
       (list)]

      ;; declare-refinement
      ;; FIXME - this sucks and should die
      [t:type-refinement
       (match (lookup-type/lexical #'t.predicate)
              [(and t (Function: (list (arr: (list dom) (Values: (list (Result: rng _ _))) #f #f '()))))
               (let ([new-t (make-pred-ty (list dom)
                                          rng
                                          (make-Refinement dom #'t.predicate))])
                 (register-type #'t.predicate new-t))
               (list)]
              [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]

      ;; require/typed
      [r:typed-require
       (let ([t (parse-type #'r.type)])
         (register-type #'r.name t)
         (list (make-def-binding #'r.name t)))]

      [r:typed-require/struct
       (let* ([t (parse-type #'r.type)]
              [flds (map fld-t (Struct-flds (lookup-type-name (Name-id t))))]
              [mk-ty (flds #f . ->* . t)])
         (register-type #'r.name mk-ty)
         (list (make-def-binding #'r.name mk-ty)))]

      ;; define-typed-struct (handled earlier)
      [(~or _:typed-struct _:typed-struct/exec)
       (list)]

      ;; predicate assertion - needed for define-type b/c or doesn't work
      [p:predicate-assertion
       (register-type #'p.predicate (make-pred-ty (parse-type #'p.type)))
       (list)]

      ;; top-level type annotation
      [t:type-declaration
       (register-type/undefined #'t.id (parse-type #'t.type))
       (register-scoped-tvars #'t.id (parse-literal-alls #'t.type))
       (list)]


      ;; values definitions
      [(define-values (var ...) expr)
       (let* ([vars (syntax->list #'(var ...))])
         (cond
           ;; if all the variables have types, we stick them into the environment
           [(andmap type-label-property vars)
            (let ([ts (map (λ (x) (get-type x #:infer #f)) vars)])
              (for-each register-type-if-undefined vars ts)
              (map make-def-binding vars ts))]
           ;; if this already had an annotation, we just construct the binding reps
           [(andmap (lambda (s) (lookup-type s (lambda () #f))) vars)
            (define top-level? (eq? (syntax-local-context) 'top-level))
            (for ([var (in-list vars)])
              (when (dict-has-key? unann-defs var)
                (free-id-table-remove! unann-defs var))
              (finish-register-type var top-level?))
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
       (apply append (stx-map tc-toplevel/pass1 #'rest))]

      ;; define-syntaxes just get noted
      [(define-syntaxes (var:id ...) . rest)
       (stx-map make-def-stx-binding #'(var ...))]

      ;; otherwise, do nothing in this pass
      ;; handles expressions, provides, requires, etc and whatnot
      [_ (list)])))





;; typecheck the expressions of a module-top-level form
;; no side-effects
;; syntax? -> (or/c void? tc-results/c)
(define (tc-toplevel/pass2 form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literal-sets (kernel-literals)
      ;; these forms we have been instructed to ignore
      [stx
       #:when (ignore-property form)
       (void)]

      ;; this is a form that we mostly ignore, but we check some interior parts
      [stx
       #:when (ignore-some-property form)
       (check-subforms/ignore form)]

      ;; these forms should always be ignored
      [((~or define-syntaxes begin-for-syntax #%require #%provide #%declare) . _) (void)]

      ;; submodules take care of themselves:
      [(module n spec (#%plain-module-begin body ...)) (void)]
      ;; module* is not expanded, so it doesn't have a `#%plain-module-begin`
      [(module* n spec body ...) (void)]

      ;; definitions just need to typecheck their bodies
      [(define-values () expr)
       (tc-expr/check #'expr (ret empty))]
      [(define-values (var ...) expr)
       (unless (for/and ([v (in-syntax #'(var ...))])
                 (free-id-table-ref unann-defs v (lambda _ #f)))
         (let ([ts (stx-map lookup-type #'(var ...))])
           (when (= 1 (length ts))
             (add-scoped-tvars #'expr (lookup-scoped-tvars (stx-car #'(var ...)))))
           (tc-expr/check #'expr (ret ts))))
       (void)]

      ;; to handle the top-level, we have to recur into begins
      [(begin) (void)]
      [(begin . rest)
       (for/last ([form (in-syntax #'rest)])
         (tc-toplevel/pass2 form))]

      ;; otherwise, the form was just an expression
      [_ (tc-expr/check form tc-any-results)])))



;; new implementation of type-check
(define (parse-def x)
  (syntax-parse x
    #:literal-sets (kernel-literals)
    [(define-values (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define (parse-syntax-def x)
  (syntax-parse x
    #:literal-sets (kernel-literals)
    [(define-syntaxes (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define (parse-type-alias form)
  (syntax-parse form
    [t:type-alias (values #'t.name #'t.type)]))

;; actually do the work on a module
;; produces prelude and post-lude syntax objects
;; syntax-list -> (values syntax syntax)
(define (type-check forms0)
  (define forms (syntax->list forms0))
  (define-values (type-aliases struct-defs stx-defs0 val-defs0 provs reqs)
    (filter-multiple
     forms
     type-alias? 
     (lambda (e) (or (typed-struct? e) (typed-struct/exec? e)))
     parse-syntax-def
     parse-def
     provide?
     define/fixup-contract?))
  (do-time "Form splitting done")
  ;(printf "before parsing type aliases~n")
  (for-each (compose register-type-alias parse-type-alias) type-aliases)
  ;; Add the struct names to the type table, but not with a type
  ;(printf "before adding type names~n")
  (let ((names (map name-of-struct struct-defs))
        (type-vars (map type-vars-of-struct struct-defs)))
    (for-each register-type-name names)
    (for-each add-constant-variance! names type-vars))
  ;(printf "after adding type names~n")
  ;; resolve all the type aliases, and error if there are cycles
  (resolve-type-aliases parse-type)
  ;; Parse and register the structure types
  (define parsed-structs
    (for/list ((def (in-list struct-defs)))
      (define parsed (parse-typed-struct def))
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
                       (map tc-toplevel/pass1 forms))))
  ;(displayln "Finished pass1")
  ;; separate the definitions into structures we'll handle for provides
  (define def-tbl
    (for/fold ([h (make-immutable-free-id-table)])
      ([def (in-list defs)])
      ;; TODO figure out why without these checks some tests break
      (define (plain-stx-binding? def)
        (and (def-stx-binding? def) (not (def-struct-stx-binding? def))))
      (define (merge-def-bindings other-def)
        (cond
          [(not other-def) def]
          [(plain-stx-binding? def) other-def]
          [(plain-stx-binding? other-def) def]
          [else
            (int-err "Two conflicting definitions: ~a ~a" def other-def)]))
      (dict-update h (binding-name def) merge-def-bindings #f)))
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
         (for/fold ([h h]) ([f (in-syntax #'(form ...))])
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
	      (#%plain-module-begin ;; avoid top-level printing and config
	       (require typed-racket/types/numeric-tower typed-racket/env/type-name-env
			typed-racket/env/global-env typed-racket/env/type-alias-env
			typed-racket/types/struct-table typed-racket/types/abbrev
			(rename-in racket/private/sort [sort raw-sort]))
	       #,(env-init-code syntax-provide? provide-tbl def-tbl)
	       #,(talias-env-init-code)
	       #,(tname-env-init-code)
	       #,(tvariance-env-init-code)
	       #,(mvar-env-init-code mvar-env)
	       #,(make-struct-table-code)
               #,@(for/list ([a (in-list aliases)])
                    (match a
                      [(list from to)
                       #`(add-alias (quote-syntax #,from) (quote-syntax #,to))])))))
	  (begin-for-syntax (add-mod! (variable-reference->module-path-index
				       (#%variable-reference)))))
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
;; syntax -> (values #f (or/c void? tc-results/c))
(define (tc-toplevel-form form)
  (syntax-parse form
    [((~literal begin) e ...)
     ;; Don't open up `begin`s that are supposed to be ignored
     #:when (not (or (ignore-property form) (ignore-some-property form)))
     (define result
       (for/last ([form (in-syntax #'(e ...))])
         (define-values (_ result) (tc-toplevel-form form))
         result))
     (begin0 (values #f result)
             (report-all-errors))]
    [_
     ;; Handle type aliases
     (when (type-alias? form)
       ((compose register-type-alias parse-type-alias) form))
     ;; Handle struct definitions
     (when (typed-struct? form)
       (define name (name-of-struct form))
       (define tvars (type-vars-of-struct form))
       (register-type-name name)
       (add-constant-variance! name tvars)
       (define parsed (parse-typed-struct form))
       (register-parsed-struct-sty! parsed)
       (refine-struct-variance! (list parsed))
       (register-parsed-struct-bindings! parsed))
     (tc-toplevel/pass1 form)
     (begin0 (values #f (tc-toplevel/pass2 form))
             (report-all-errors))]))

