#lang racket/base
(require "../common/performance.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/property.rkt"
         "../syntax/match.rkt"
         "../common/phase.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../expand/root-expand-context.rkt"
         "../expand/parsed.rkt"
         "../common/module-path.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "built-in-symbol.rkt"
         "../host/linklet.rkt"
         "../host/correlate.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "namespace-scope.rkt"
         "expr.rkt"
         "extra-inspector.rkt"
         "correlate.rkt"
         "correlated-linklet.rkt")

(provide compile-forms
         compile-module-linklet
         compile-namespace-scopes)

(struct link-info (link-module-uses imports extra-inspectorsss def-decls))

;; Compiles a module body or sequence of top-level forms, returning a
;; linklet directory to cover all phases covered by the forms
(define (compile-forms bodys cctx mpis
                       #:body-imports body-imports
                       #:body-import-instances body-import-instances
                       #:body-suffix-forms [body-suffix-forms null]
                       #:force-phases [force-phases null]
                       #:encoded-root-expand-ctx-box [encoded-root-expand-ctx-box #f] ; encoded root context, if any
                       #:root-ctx-only-if-syntax? [root-ctx-only-if-syntax? #f]
                       #:compiled-expression-callback [compiled-expression-callback void]
                       #:definition-callback [definition-callback void]
                       #:other-form-callback [other-form-callback void]
                       #:get-module-linklet-info [get-module-linklet-info (lambda (mod-name p) #f)] ; to support submodules
                       #:serializable? [serializable? #t]
                       #:module-prompt? [module-prompt? #f]
                       #:to-correlated-linklet? [to-correlated-linklet? #f]
                       #:cross-linklet-inlining? [cross-linklet-inlining? #t])
  (define phase (compile-context-phase cctx))
  (define self (compile-context-self cctx))
  
  ;; Accumulate syntax objects across all phases:
  (define syntax-literals (make-syntax-literals))

  ;; For each phase, keep track of all compiled expressions for the
  ;; phase
  (define phase-to-body (make-hasheqv)) ; phase -> list of S-expression
  (define (add-body! phase body)
    (hash-update! phase-to-body phase (lambda (l) (cons body l)) null))

  ;; For each phase, accumulate a header for referenced imports and
  ;; syntax literals
  (define phase-to-header (make-hasheqv)) ; phase -> header
  (define (find-or-create-header! phase)
    (or (hash-ref phase-to-header phase #f)
        (let ([header (make-header mpis syntax-literals)])
          (hash-set! phase-to-header phase header)
          header)))

  ;; Ensure that some requested phases are realized:
  (for ([phase (in-list force-phases)])
    (find-or-create-header! phase)
    (add-body! phase '(void)))

  ;; Keep track of whether any `define-syntaxes` appeared at any phase
  (define saw-define-syntaxes? #f)

  (when (compile-context-module-self cctx)
    ;; In a module, select non-conflicting symbols for definitions,
    ;; first, in the hope that we can just the names as-is; and we'll
    ;; rename locals as needed to avoid these names
    (let loop! ([bodys bodys] [phase phase] [header (find-or-create-header! phase)])
      (for ([body (in-list bodys)])
        (cond
         [(parsed-define-values? body)
          (for ([sym (in-list (parsed-define-values-syms body))])
            (define def-sym (select-fresh sym header))
            (hash-set! (header-binding-sym-to-define-sym header)
                       sym
                       def-sym)
            (set-header-binding-syms-in-order! header
                                               (cons sym
                                                     (header-binding-syms-in-order header)))
            (register-as-defined! header def-sym))]
         [(parsed-begin-for-syntax? body)
          (loop! (parsed-begin-for-syntax-body body) (add1 phase) (find-or-create-header! (add1 phase)))]))))
  
  ;; Provided for callbacks to detect required references:
  (define ((as-required? header) sym)
    (registered-as-required? header sym))

  ;; Compile each form in `bodys`, recording results in `phase-to-body`
  (define last-i (sub1 (length bodys)))
  (let loop! ([bodys bodys] [phase phase] [header (find-or-create-header! phase)])
    (for ([body (in-list bodys)]
          [i (in-naturals)])
      (cond
       [(parsed-define-values? body)
        (define ids (parsed-define-values-ids body))
        (define binding-syms (parsed-define-values-syms body))
        (define def-syms
          (cond
           [(compile-context-module-self cctx)
            ;; In a module, look up name for local definition:
            (for/list ([binding-sym (in-list binding-syms)]
                       [id (in-list ids)])
              (correlate-source-name
               (hash-ref (header-binding-sym-to-define-sym header)
                         binding-sym)
               (syntax-e id)))]
           [else
            ;; Outside of a module, look up name to `set!`
            (for/list ([binding-sym (in-list binding-syms)])
              (register-required-variable-use! header
                                               (compile-context-self cctx)
                                               phase
                                               binding-sym
                                               #f
                                               #:defined? #t))]))
        (define rhs (compile (parsed-define-values-rhs body)
                             (struct-copy compile-context cctx
                                          [phase phase]
                                          [header header])
                             (and (= (length ids) 1) (car ids))))
        (definition-callback)
        (compiled-expression-callback rhs (length def-syms) phase (as-required? header))
        ;; Generate a definition:
        (add-body! phase (propagate-inline-property
                          (correlate* (parsed-s body) `(define-values ,def-syms ,rhs))
                          (parsed-s body)))
        (unless (or (compile-context-module-self cctx)
                    (null? ids))
          ;; Not in a module; ensure that the defined names are
          ;; treated as mutable
          (add-body! phase
                     `(if #f
                       (begin
                         ,@(for/list ([def-sym (in-list def-syms)])
                             `(set! ,def-sym #f)))
                       (void)))
          ;; Also, install a binding at run time
          (add-body! phase (compile-top-level-bind
                            ids binding-syms
                            (struct-copy compile-context cctx
                                         [phase phase]
                                         [header header])
                            #f)))]
       [(parsed-define-syntaxes? body)
        (define ids (parsed-define-syntaxes-ids body))
        (define binding-syms (parsed-define-syntaxes-syms body))
        (define next-header (find-or-create-header! (add1 phase)))
        (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                           (define gen-sym (select-fresh binding-sym next-header))
                           (register-as-defined! next-header gen-sym)
                           gen-sym))
        (define rhs (compile (parsed-define-syntaxes-rhs body)
                             (struct-copy compile-context cctx
                                          [phase (add1 phase)]
                                          [header next-header])))
        (definition-callback)
        (compiled-expression-callback rhs (length gen-syms) (add1 phase) (as-required? header))
        (define transformer-set!s (for/list ([binding-sym (in-list binding-syms)]
                                             [gen-sym (in-list gen-syms)])
                                    `(,set-transformer!-id ',binding-sym ,gen-sym)))
        (cond
         [(compile-context-module-self cctx)
          (add-body! (add1 phase) `(let-values ([,gen-syms ,rhs])
                                    (begin
                                      ,@transformer-set!s
                                      (void))))]
         [else
          (add-body! (add1 phase)
                     (generate-top-level-define-syntaxes
                      gen-syms rhs transformer-set!s
                      (compile-top-level-bind
                       ids binding-syms
                       (struct-copy compile-context cctx
                                    [phase phase]
                                    [header header])
                       gen-syms)))])
        (set! saw-define-syntaxes? #t)]
       [(parsed-begin-for-syntax? body)
        (loop! (parsed-begin-for-syntax-body body) (add1 phase) (find-or-create-header! (add1 phase)))]
       [(or (parsed-#%declare? body) (parsed-module? body) (parsed-require? body))
        ;; Must be handled separately, if allowed at all
        (define e (other-form-callback body (struct-copy compile-context cctx
                                                         [phase phase]
                                                         [header header])))
        (when e
          (compiled-expression-callback e #f phase (as-required? header))
          (add-body! phase e))]
       [else
        (define e (compile body
                           (struct-copy compile-context cctx
                                        [phase phase]
                                        [header header])
                           #f
                           (= i last-i)))
        (compiled-expression-callback e #f phase (as-required? header))
        (add-body! phase e)])))

  ;; Register root-expand-context, if any, encoded as a syntax object;
  ;; see also "../eval/root-context.rkt"
  (define encoded-root-expand-pos
    (and encoded-root-expand-ctx-box
         (unbox encoded-root-expand-ctx-box) ; box => can be cleared by a callback
         (not (and root-ctx-only-if-syntax?
                   (not saw-define-syntaxes?)
                   (syntax-literals-empty? syntax-literals)))
         (add-syntax-literal! syntax-literals (unbox encoded-root-expand-ctx-box))))

  ;; Collect resulting phases
  (define phases-in-order (sort (hash-keys phase-to-body) <))
  (define min-phase (if (pair? phases-in-order)
                        (car phases-in-order)
                        phase))
  (define max-phase (if (pair? phases-in-order)
                        (car (reverse phases-in-order))
                        phase))

  ;; Compute linking info for each phase
  (define phase-to-link-info
    (for/hash ([phase (in-list phases-in-order)])
      (define header (hash-ref phase-to-header phase #f))
      (define-values (link-module-uses imports extra-inspectorsss def-decls)
        (generate-links+imports header phase cctx cross-linklet-inlining?))
      (values phase (link-info link-module-uses imports extra-inspectorsss def-decls))))
  
  ;; Generate the phase-specific linking units
  (define body-linklets+module-use*s
    (for/hasheq ([phase (in-list phases-in-order)])
      (define bodys (hash-ref phase-to-body phase))
      (define li (hash-ref phase-to-link-info phase))
      (define binding-sym-to-define-sym
        (header-binding-sym-to-define-sym (hash-ref phase-to-header phase)))
      (define module-use*s
        (module-uses-add-extra-inspectorsss (link-info-link-module-uses li)
                                            (link-info-extra-inspectorsss li)))
      (define body-linklet
        `(linklet
             ;; imports
             (,@body-imports
              ,@(link-info-imports li))
             ;; exports
             (,@(link-info-def-decls li)
              ,@(for/list ([binding-sym (in-list (header-binding-syms-in-order
                                                  (hash-ref phase-to-header phase)))])
                  (define def-sym (hash-ref binding-sym-to-define-sym binding-sym))
                  (if (eq? def-sym binding-sym)
                      def-sym
                      `[,def-sym ,binding-sym])))
           ;; body
           ,@(reverse bodys)
           ,@body-suffix-forms))
      (define-values (linklet new-module-use*s)
        (cond
          [to-correlated-linklet?
           (values (make-correlated-linklet body-linklet 'module) module-use*s)]
          [else
           ;; Compile the linklet with support for cross-module inlining, which
           ;; means that the set of imports can change:
           (compile-module-linklet body-linklet
                                   #:body-imports body-imports
                                   #:body-import-instances body-import-instances
                                   #:get-module-linklet-info get-module-linklet-info
                                   #:serializable? serializable?
                                   #:module-prompt? module-prompt?
                                   #:module-use*s module-use*s
                                   #:cross-linklet-inlining? cross-linklet-inlining?
                                   #:load-modules? #f
                                   #:namespace (compile-context-namespace cctx))]))
      (values phase (cons linklet new-module-use*s))))
  
  (define body-linklets
    (for/hasheq ([(phase l+mu*s) (in-hash body-linklets+module-use*s)])
      (values phase (car l+mu*s))))

  (define phase-to-link-module-uses
    (for/hasheq ([(phase l+mu*s) (in-hash body-linklets+module-use*s)])
      (values phase (module-uses-strip-extra-inspectorsss (cdr l+mu*s)))))

  (define phase-to-link-module-uses-expr
    (serialize-phase-to-link-module-uses phase-to-link-module-uses mpis))

  (define phase-to-link-extra-inspectorsss
    (for*/hash ([(phase l+mu*s) (in-hash body-linklets+module-use*s)]
                [(extra-inspectorsss) (in-value (module-uses-extract-extra-inspectorsss
                                                 (cdr l+mu*s)
                                                 (car l+mu*s)
                                                 (and cross-linklet-inlining?
                                                      (not to-correlated-linklet?))
                                                 (length body-imports)))]
                #:when extra-inspectorsss)
      (values phase extra-inspectorsss)))

  (values body-linklets   ; main compilation result
          min-phase
          max-phase
          phase-to-link-module-uses
          phase-to-link-module-uses-expr
          phase-to-link-extra-inspectorsss
          syntax-literals
          encoded-root-expand-pos))

;; ----------------------------------------

;; Evaluating a top-level definition has a secondary effect: it
;; adjusts the binding of defined identifiers. This mingling of
;; evaluation and expansion is the main weirdness of the top
;; level.
(define (compile-top-level-bind ids binding-syms cctx trans-exprs)
  (define phase (compile-context-phase cctx))
  (define self (compile-context-self cctx))
  (define header (compile-context-header cctx))
  (define mpis (header-module-path-indexes header))
  ;; The binding that we install at run time should not include
  ;; the temporary binding scope that the expander added:
  (define top-level-bind-scope (root-expand-context-top-level-bind-scope
                                (namespace-get-root-expand-ctx
                                 (compile-context-namespace cctx))))
  ;; For installing a binding:
  (define self-expr (add-module-path-index! mpis self))
  ;; Generate calls to `top-level-bind!`:
  `(begin
    ,@(for/list ([id (in-list ids)]
                 [binding-sym (in-list binding-syms)]
                 [trans-expr (in-list (or trans-exprs
                                          (for/list ([id (in-list ids)])
                                            `'#f)))])
        (define id-stx
          (compile-quote-syntax (remove-scope id top-level-bind-scope)
                                cctx))
        `(,top-level-bind!-id ,id-stx ,self-expr ,phase ,phase-shift-id ,ns-id ',binding-sym
          ,(and trans-exprs #t) ,trans-expr))))

;; To support namespace-relative binding, bundle scope information for
;; the current namespace into a syntax object
(define (compile-namespace-scopes cctx)
  (define v (encode-namespace-scopes (compile-context-namespace cctx)))
  (compile-quote-syntax v cctx))

;; ----------------------------------------

;; Handle the `define-syntaxes`-with-zero-results hack for the top level;
;; beware that we make two copies of `finish`
(define (generate-top-level-define-syntaxes gen-syms rhs transformer-set!s finish)
  `(call-with-values
    (lambda () ,rhs)
    (case-lambda
      [,gen-syms
       (begin
         ,@transformer-set!s
         ,finish
         (void))]
      [()
       (let-values ([,gen-syms (values ,@(for/list ([s (in-list gen-syms)]) `'#f))])
         (begin
           ,finish
           (void)))]
      [args
       ;; Provoke the wrong-number-of-arguments error:
       (let-values ([,gen-syms (apply values args)])
         (void))])))

;; ----------------------------------------

(define (propagate-inline-property e orig-s)
  (define v (syntax-property orig-s 'compiler-hint:cross-module-inline))
  (if v
      (correlated-property e 'compiler-hint:cross-module-inline v)
      e))

;; ----------------------------------------

;; Compile the linklet with support for cross-module inlining, which
;; means that the set of imports can change: return a compiled linklet
;; and a list of `module-use*`
(define (compile-module-linklet body-linklet
                                #:compile-linklet [compile-linklet compile-linklet]
                                #:body-imports body-imports
                                #:body-import-instances body-import-instances
                                #:get-module-linklet-info get-module-linklet-info
                                #:serializable? serializable?
                                #:module-prompt? module-prompt?
                                #:module-use*s module-use*s
                                #:cross-linklet-inlining? cross-linklet-inlining?
                                #:load-modules? load-modules?
                                #:namespace namespace)
  (define-values (linklet new-module-use*s)
    (performance-region
     ['compile '_ 'linklet]
     ((lambda (l name keys getter)
        (compile-linklet l name keys getter (if serializable?
                                                (if module-prompt?
                                                    '(serializable use-prompt)
                                                    '(serializable))
                                                (if module-prompt?
                                                    '(use-prompt)
                                                    '()))))
      body-linklet
      'module
      ;; Support for cross-module optimization starts with a vector
      ;; of keys for the linklet imports; we use `module-use` values
      ;; as keys, plus #f or an instance (=> cannot be pruned) for
      ;; each boilerplate linklet
      (list->vector (append body-import-instances
                            module-use*s))
      ;; To complete cross-module support, map a key (which is a `module-use`)
      ;; to a linklet and an optional vector of keys for that linklet's
      ;; imports:
      (make-module-use-to-linklet cross-linklet-inlining?
                                  load-modules?
                                  namespace
                                  get-module-linklet-info
                                  module-use*s))))
  (values linklet (list-tail (vector->list new-module-use*s)
                             (length body-imports))))

;; ----------------------------------------

(define (make-module-use-to-linklet cross-linklet-inlining? load-modules?
                                    ns get-module-linklet-info init-mu*s)
  ;; Inlining might reach the same module though different indirections;
  ;; use a consistent `module-use` value so that the compiler knows to
  ;; collapse them to a single import
  (define mu*-intern-table (make-hash))
  (define (intern-module-use* mu*)
    (define mod-name (module-path-index-resolve (module-use-module mu*)))
    (define existing-mu* (hash-ref mu*-intern-table (cons mod-name (module-use-phase mu*)) #f))
    (cond
      [existing-mu*
       (module-use-merge-extra-inspectorss! existing-mu* mu*)
       existing-mu*]
      [else
       (hash-set! mu*-intern-table (cons mod-name (module-use-phase mu*)) mu*)
       mu*]))
  (for ([mu* (in-list init-mu*s)])
    (intern-module-use* mu*))
  ;; The callback function supplied to `compile-linklet`:
  (lambda (mu*-or-instance)
    (cond
     [(instance? mu*-or-instance)
      ;; An instance represents a boilerplate linklet. An instance
      ;; doesn't enable inlining (and we don't want inlining, since
      ;; that would change the overall protocol for module or
      ;; top-level linklets), but it can describe shapes.
      (values mu*-or-instance #f)]
     [(not cross-linklet-inlining?)
      ;; Although we let instances through, because that's cheap,
      ;; don't track down linklets and allow inlining of functions
      (values #f #f)]
     [mu*-or-instance
      (define mu* mu*-or-instance)
      (define mod-name (module-path-index-resolve (module-use-module mu*) load-modules?))
      (define mli (or (get-module-linklet-info mod-name (module-use-phase mu*))
                      (namespace->module-linklet-info ns
                                                      mod-name
                                                      (module-use-phase mu*))))
      (when mli
        ;; Record the module's declaration-time inspector, for use
        ;; later recording extra inspectors for inlined referenced
        (module-use*-declaration-inspector! mu* (module-linklet-info-inspector mli)))
      (if mli
          ;; Found info for inlining:
          (values (module-linklet-info-linklet-or-instance mli)
                  (and (module-linklet-info-module-uses mli) ; => linklet
                       (list->vector
                        (append
                         '(#f #f) ; boilerplate imports common to all modules
                         (let ([mus (module-linklet-info-module-uses mli)]
                               [extra-inspectorsss (module-linklet-info-extra-inspectorsss mli)])
                           (for/list ([sub-mu (in-list mus)]
                                      [imports (in-list
                                                (linklet-import-variables
                                                 (module-linklet-info-linklet-or-instance mli)))]
                                      [extra-inspectorss (in-list (or extra-inspectorsss
                                                                      ;; a list of the right length:
                                                                      mus))])
                             (intern-module-use*
                              (module-use+extra-inspectors (module-path-index-shift
                                                            (module-use-module sub-mu)
                                                            (module-linklet-info-self mli)
                                                            (module-use-module mu*))
                                                           (module-use-phase sub-mu)
                                                           ;; The remaining arguments are used to
                                                           ;; make an `module-use*` instead of a
                                                           ;; plain `module-use`
                                                           imports
                                                           (module-linklet-info-inspector mli)
                                                           (module-linklet-info-extra-inspector mli)
                                                           (and extra-inspectorsss
                                                                extra-inspectorss)))))))))
          ;; Didn't find info, for some reason:
          (values #f #f))]
     [else
      ;; Boilerplate linklet with no compile-time information
      (values #f #f)])))
