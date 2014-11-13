#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/format
         syntax/stx
         "../util/eomap.rkt"
         "deriv-util.rkt"
         "deriv.rkt"
         "reductions-engine.rkt")

(provide reductions
         reductions+)

;; Reductions

;; reductions : WDeriv -> ReductionSequence
(define (reductions d)
  (let-values ([(steps binders definites estx exn) (reductions+ d)])
    steps))

;; Binders = hasheq[identifier => phase-level]
;; Definites = eomap[identifier => phase-level]

;; reductions+ : WDeriv -> (list-of step) Binders Definites ?stx ?exn
(define (reductions+ d)
  (parameterize ((current-definites (empty-eomap))
                 (current-binders #hasheq())
                 (current-frontier null)
                 (hides-flags (list (box #f)))
                 (sequence-number 0))
    (RScase ((Expr d) (wderiv-e1 d) (wderiv-e1 d) #f null)
            (lambda (steps stx vstx s)
              (values (reverse steps) (current-binders) (current-definites) vstx #f))
            (lambda (steps exn)
              (values (reverse steps) (current-binders) (current-definites) #f exn)))))

;; Syntax

(define-syntax-rule (match/count x clause ...)
  (begin (sequence-number (add1 (sequence-number)))
         (let ([v x])
           (match v
             clause ...
             [_ (error 'match "failed to match ~e at line ~s" v (line-of x))]))))

(define-syntax (line-of stx)
  (syntax-case stx ()
    [(line-of x) #`(quote #,(syntax-line #'x))]))

;; Derivations => Steps

;; Expr : Deriv -> RST
(define (Expr d)
  (match/count d
    [(Wrap deriv (e1 e2))
     (R [#:pattern ?form]
        [#:let transparent-stx (hash-ref opaque-table (syntax-e #'?form) #f)]
        [#:when transparent-stx
                [#:set-syntax transparent-stx]]
        [#:expect-syntax e1 (list d)]
        [#:when (base? d)
                [#:learn (or (base-resolves d) null)]]
        [#:seek-check]
        [Expr* ?form d]
        [#:when (not (current-pass-hides?))
                [#:set-syntax e2]])]
    [#f
     (R [#:seek-check]
        => (Expr* d))]))

(define (Expr* d)
  (match d
    ;; Primitives
    [(Wrap p:variable (e1 e2 rs ?1))
     (R [#:learn (list e2)]
        [#:when (or (not (identifier? e1))
                    (not (bound-identifier=? e1 e2)))
                [#:walk e2 'resolve-variable]])]
    [(Wrap p:module (e1 e2 rs ?1 prep tag rename check tag2 ?3 body shift))
     (R [#:hide-check rs]
        [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?module ?name ?language . ?body-parts)]
        [#:when tag
                [#:in-hole ?body-parts
                           [#:walk (list tag) 'tag-module-begin]]]
        [#:pattern (?module ?name ?language ?body)]
        [#:rename ?body rename]
        [#:pass1]
        [#:when check
                [Expr ?body check]]
        [#:when tag2
                [#:in-hole ?body
                           [#:walk tag2 'tag-module-begin]]]
        [#:pass2]
        [! ?3]
        [Expr ?body body]
        [#:pattern ?form]
        [#:rename ?form shift])]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 me body ?2 subs))
     (R [! ?1]
        [#:pattern ?form]
        [#:rename ?form me]
        [#:pattern (?module-begin . ?forms)]
        [ModuleBegin/Phase ?forms body]
        [! ?2]
        [Submodules ?forms subs])]
    [(Wrap p:define-syntaxes (e1 e2 rs ?1 prep rhs locals))
     (R [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?define-syntaxes ?vars ?rhs)]
        [#:binders #'?vars]
        [Expr/PhaseUp ?rhs rhs]
        [LocalActions ?rhs locals])]
    [(Wrap p:define-values (e1 e2 rs ?1 rhs))
     (R [! ?1]
        [#:pattern (?define-values ?vars ?rhs)]
        [#:binders #'?vars]
        [#:when rhs
                [Expr ?rhs rhs]])]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner #f))
     (R [! ?1]
        [#:pattern (?expr-kw ?inner)]
        [Expr ?inner inner])]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner untag))
     (R [! ?1]
        [#:pattern (?expr-kw ?inner)]
        [#:pass1]
        [Expr ?inner inner]
        [#:pattern ?form]
        [#:let oldform #'?form]
        [#:with-visible-form
         [#:left-foot]
         [#:set-syntax (stx-car (stx-cdr #'?form))]
         [#:step 'macro]] ;; FIXME: 'untag-expr
        [#:pass2]
        [#:set-syntax (stx-car (stx-cdr oldform))]
        [#:rename ?form untag])]
    [(Wrap p:if (e1 e2 rs ?1 test then else))
     (R [! ?1]
        [#:pattern (?if TEST THEN ELSE)]
        [Expr TEST test]
        [Expr THEN then]
        [Expr ELSE else])]
    [(Wrap p:wcm (e1 e2 rs ?1 key mark body))
     (R [! ?1]
        [#:pattern (?wcm KEY MARK BODY)]
        [Expr KEY key]
        [Expr MARK mark]
        [Expr BODY body])]
    [(Wrap p:begin (e1 e2 rs ?1 lderiv))
     (R [! ?1]
        [#:pattern (?begin . ?lderiv)]
        [List ?lderiv lderiv])]
    [(Wrap p:begin0 (e1 e2 rs ?1 first lderiv))
     (R [! ?1]
        [#:pattern (?begin0 FIRST . LDERIV)]
        [Expr FIRST first]
        [List LDERIV lderiv])]
    [(Wrap p:#%app (e1 e2 rs ?1 lderiv))
     (R [! ?1]
        [#:pattern (?app . LDERIV)]
        [#:if lderiv
              ([List LDERIV lderiv])
              ([#:walk e2 'macro])])]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (R [! ?1]
        [#:pattern (?lambda ?formals . ?body)]
        [#:rename (?formals . ?body) renames 'rename-lambda]
        [#:binders #'?formals]
        [Block ?body body])]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (R [! ?1]
        [#:pattern (?case-lambda . ?clauses)]
        [CaseLambdaClauses ?clauses clauses])]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (R [! ?1]
        [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
        [#:rename (((?vars ?rhs) ...) . ?body) renames 'rename-let-values]
        [#:binders #'(?vars ...)]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (R [! ?1]
        [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
        [#:rename (((?vars ?rhs) ...) . ?body) renames 'rename-letrec-values]
        [#:binders #'(?vars ...)]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-syntaxes+values
           (e1 e2 rs ?1 srenames prep srhss vrenames vrhss body tag))
     (R [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pass1]
        [#:pattern (?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body)]
        [#:rename (((?svars ?srhs) ...) ((?vvars ?vrhs) ...) . ?body)
                   srenames
                   'rename-lsv]
        [#:binders #'(?svars ... ?vvars ...)]
        [#:when (pair? srhss) ;; otherwise, we're coming from a block expansion
                [BindSyntaxes (?srhs ...) srhss]]
        ;; If vrenames is #f, no var bindings to rename
        [#:when vrenames
                [#:rename (((?vvars ?vrhs) ...) . ?body) vrenames 'rename-lsv]
                [#:binders #'(?vvars ...)]]
        [Expr (?vrhs ...) vrhss]
        [Block ?body body]
        [#:pass2]
        [#:pattern ?form]
        [#:when tag
                [#:walk tag 'lsv-remove-syntax]])]
    [(Wrap p:#%datum (e1 e2 rs ?1))
     (R [! ?1]
        [#:hide-check rs]
        [#:walk e2 'macro])]
    [(Wrap p:#%top (e1 e2 rs ?1))
     (R [! ?1]
        [#:pattern ?form]
        [#:learn
         (syntax-case #'?form ()
           [(?top . ?var) (identifier? #'?var) (list #'?var)]
           [?var (identifier? #'?var) (list #'?var)]
           [_ (error 'macro-debugger "#%top has wrong form: ~s\n" #'?form)])])]

    [(Wrap p:provide (e1 e2 rs ?1 inners ?2))
     (let ([wrapped-inners (map expr->local-action inners)])
       (R [! ?1]
          [#:pattern ?form]
          [#:pass1]
          [#:left-foot]
          [LocalActions ?form wrapped-inners]
          [! ?2]
          [#:pass2]
          [#:set-syntax e2]
          [#:step 'provide]
          [#:set-syntax e2]))]

    [(Wrap p:require (e1 e2 rs ?1 locals))
     (R [! ?1]
        [#:pattern ?form]
        [LocalActions ?form locals])]

    [(Wrap p:#%stratified-body (e1 e2 rs ?1 bderiv))
     (R [! ?1]
        [#:pass1]
        [#:pattern (?sb . ?body)]
        [Block ?body bderiv]
        [#:pass2]
        [#:hide-check rs]
        [#:pattern ?form]
        [#:walk e2 'macro])]

    [(Wrap p:submodule* (e1 e2 rs ?1))
     (R [! ?1])]
    [(Wrap p:submodule (e1 e2 rs ?1 exp))
     (R [! ?1]
        [#:pattern ?form]
        [Expr ?form exp])]

    [(Wrap p:stop (e1 e2 rs ?1))
     (R [! ?1])]

    ;; The rest of the automatic primitives
    [(Wrap p::STOP (e1 e2 rs ?1))
     (R [! ?1])]

    [(Wrap p:set!-macro (e1 e2 rs ?1 deriv))
     (R [! ?1]
        [#:pattern ?form]
        [Expr ?form deriv])]
    [(Wrap p:set! (e1 e2 rs ?1 id-rs ?2 rhs))
     (R [! ?1]
        [#:pattern (?set! ?var ?rhs)]
        [#:learn id-rs]
        [! ?2]
        [Expr ?rhs rhs])]

    [(Wrap p:begin-for-syntax (e1 e2 rs ?1 prep body locals))
     (R [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?bfs . ?forms)]
        [#:parameterize ((phase (add1 (phase))))
          [#:if (module-begin/phase? body)
                [[ModuleBegin/Phase ?forms body]]
                [[BeginForSyntax ?forms body]]]]
        [LocalActions ?forms locals])]

    ;; Macros
    [(Wrap mrule (e1 e2 rs ?1 me1 locals me2 ?2 etx next))
     (R [! ?1]
        [#:pattern ?form]
        [#:hide-check rs]
        [#:learn rs]
        [#:pass1]
        [#:left-foot]
        [#:rename/mark ?form e1 me1] ;; MARK
        [LocalActions ?form locals]
        [! ?2]
        [#:pass2]
        [#:set-syntax me2]
        [#:rename/unmark ?form me2 etx] ;; UNMARK
        [#:step 'macro]
        [#:set-syntax etx]
        [Expr ?form next])]

    [(Wrap tagrule (e1 e2 tagged-stx next))
     (R [#:pattern ?form]
        [#:hide-check (list (stx-car tagged-stx))]
        [#:walk tagged-stx
                (case (syntax-e (stx-car tagged-stx))
                  ((#%app) 'tag-app)
                  ((#%datum) 'tag-datum)
                  ((#%top) 'tag-top)
                  (else
                   (error 'reductions "unknown tagged syntax: ~s" tagged-stx)))]
        [Expr ?form next])]

    ;; expand/compile-time-evals

    [(Wrap ecte (e1 e2 locals first second locals2))
     (R [#:pattern ?form]
        [#:pass1]
        [LocalActions ?form locals]
        [Expr ?form first]
        [#:pass2]
        [Expr ?form second]
        [LocalActions ?form locals2])]

    ;; Lifts

    [(Wrap lift-deriv (e1 e2 first lifted-stx second))
     (R [#:pattern ?form]
        ;; lifted-stx has form (begin lift-n ... lift-1 orig-expr)
        [#:let avail (cdr (reverse (stx->list (stx-cdr lifted-stx))))]
        [#:parameterize ((available-lift-stxs avail)
                         (visible-lift-stxs null))
          [#:pass1]
          [Expr ?form first]
          [#:do (when (pair? (available-lift-stxs))
                  (lift-error 'lift-deriv "available lifts left over"))]
          [#:with-visible-form
           ;; If no lifts visible, then don't show begin-wrapping
           [#:when (pair? (visible-lift-stxs))
                   [#:walk (reform-begin-lifts lifted-stx
                                               (visible-lift-stxs)
                                               #'?form)
                           'capture-lifts]]]
          [#:pass2]
          [#:set-syntax lifted-stx]
          [Expr ?form second]])]

    [(Wrap lift/let-deriv (e1 e2 first lifted-stx second))
     (R [#:pattern ?form]
        ;; lifted-stx has form
        ;; (let-values ((last-v last-lifted))
        ;;   ...
        ;;     (let-values ((first-v first-lifted)) orig-expr))
        [#:let avail lifted-stx]
        [#:parameterize ((available-lift-stxs avail)
                         (visible-lift-stxs null))
          [#:pass1]
          [Expr ?form first]
          [#:let visible-lifts (visible-lift-stxs)]
          [#:with-visible-form
           [#:left-foot]
           [#:set-syntax (reform-let-lifts lifted-stx visible-lifts #'?form)]
           [#:step 'capture-lifts]]
          [#:pass2]
          [#:set-syntax lifted-stx]
          [Expr ?form second]])]

    ;; Skipped
    [#f
     (R)]))

;; Expr/PhaseUp : Deriv -> RST
(define (Expr/PhaseUp d)
  (R [#:parameterize ((phase (add1 (phase))))
     => (Expr* d)]))

;; case-lambda-clauses-reductions : 
;;   (list-of (W (list ?exn rename (W BDeriv)))) stxs -> RST
(define (CaseLambdaClauses clauses)
  (match/count clauses
    ['()
     (R)]
    [(cons (Wrap clc (?1 rename body)) rest)
     (R [! ?1]
        [#:pattern ((?formals . ?body) . ?rest)]
        [#:rename (?formals . ?body) rename 'rename-case-lambda]
        [#:binders #'?formals]
        [Block ?body body]
        [CaseLambdaClauses ?rest rest])]))

(define (PrepareEnv prep)
  (LocalActions prep))

;; local-actions-reductions
(define (LocalActions locals)
  (match locals
    ['()
     (R)]
    [(cons local rest)
     (R [#:pattern ?form]
        [#:parameterize ((macro-policy
                          ;; If macro with local-expand is transparent,
                          ;; then all local-expansions must be transparent.
                          (if (visibility) (lambda _ #t) (macro-policy))))
          [#:new-local-context
           [#:pattern ?form]
           [LocalAction ?form local]]]
        [LocalActions ?form rest])]))

(define (LocalAction local)
  (match/count local
    [(struct local-exn (exn))
     (R [! exn])]

    [(struct local-expansion (e1 e2 for-stx? me1 inner #f me2 opaque))
     (R [#:parameterize ((phase (if for-stx? (add1 (phase)) (phase))))
         [#:set-syntax e1]
         [#:pattern ?form]
         [#:rename/mark ?form e1 me1]
         [Expr ?form inner]
         [#:rename/mark ?form me2 e2]
         [#:do (when opaque
                 (hash-set! opaque-table (syntax-e opaque) e2))]])]

    [(struct local-expansion (e1 e2 for-stx? me1 inner lifted me2 opaque))
     (R [#:let avail
               (if for-stx?
                   lifted
                   (cdr (reverse (stx->list (stx-cdr lifted)))))]
        [#:let recombine
               (lambda (lifts form)
                 (if for-stx?
                     (reform-let-lifts lifted lifts form)
                     (reform-begin-lifts lifted lifts form)))]
        [#:parameterize ((phase (if for-stx? (add1 (phase)) (phase)))
                         (available-lift-stxs avail)
                         (visible-lift-stxs null))
         [#:set-syntax e1]
         [#:pattern ?form]
         [#:rename/unmark ?form e1 me1]
         [#:pass1]
         [Expr ?form inner]
         [#:let visible-lifts (visible-lift-stxs)]
         [#:with-visible-form
          [#:left-foot]
          [#:set-syntax (recombine visible-lifts #'?form)]
          [#:step 'splice-lifts visible-lifts]]
         [#:pass2]
         [#:set-syntax lifted]
         [#:rename/mark ?form me2 e2]
         [#:do (when opaque
                 (hash-set! opaque-table (syntax-e opaque) e2))]])]

    [(struct local-lift (expr ids))
     ;; FIXME: add action
     (R [#:do (take-lift!)]
        [#:binders ids]
        [#:reductions
         (list
          (walk/talk 'local-lift
                     (list "The macro lifted an expression"
                           ""
                           "Expression:"
                           expr
                           "Identifiers:"
                           (datum->syntax #f ids))))])]

    [(struct local-lift-end (decl))
     ;; (walk/mono decl 'module-lift)
     (R)]
    [(struct local-lift-require (req expr mexpr))
     ;; lift require
     (R [#:set-syntax expr]
        [#:pattern ?form]
        [#:rename/mark ?form expr mexpr])]
    [(struct local-lift-provide (prov))
     ;; lift provide
     (R)]
    [(struct local-bind (names ?1 renames bindrhs))
     [R [! ?1]
        ;; FIXME: use renames
        [#:binders names]
        [#:when bindrhs => (BindSyntaxes bindrhs)]]]
    [(struct track-origin (before after))
     (R)
     #|
     ;; Do nothing for now... need to account for marks also.
     [R [#:set-syntax before]
        [#:pattern ?form]
        [#:rename ?form after 'track-origin]]
     |#]
    [(struct local-value (name ?1 resolves bound? binding))
     [R [! ?1]
        ;; FIXME: notify if binding != current (identifier-binding name)???
        ;; [#:learn (list name)]
        ;; Add remark step?
        ]]
    [(struct local-remark (contents))
     (R [#:reductions (list (walk/talk 'remark contents))])]
    [(struct local-mess (events))
     ;; FIXME: While it is not generally possible to parse tokens as one or more
     ;; interrupted derivations (possibly interleaved with successful derivs),
     ;; it should be possible to recover *some* information and display it.
     (R [#:reductions
         (let ([texts
                (list (~a "Some expansion history has been lost due to a jump "
                          "within expansion.")
                      (~a "For example, a macro may have caught an "
                          "exception coming from within a call to `local-expand'."))])
           (list (walk/talk 'remark texts)))])]
    [#f
     (R)]))

(define (Submodules subs)
  (match subs
    ['()
     (R)]
    [(cons sub rest)
     (R [#:pattern ?form]
        [#:new-local-context
         [#:pattern ?form]
         [#:set-syntax (wderiv-e1 sub)]
         [Expr ?form sub]]
        [Submodules ?form rest])]))

;; List : ListDerivation -> RST
(define (List ld)
  (match ld
    [(Wrap lderiv (es1 es2 ?1 derivs))
     (R [! ?1]
        [#:pattern (?form ...)]
        [Expr (?form ...) derivs])]
    [#f
     (R)]))

;; Block  : BlockDerivation -> RST
(define (Block bd)
  (match/count bd
    [(Wrap bderiv (es1 es2 pass1 trans pass2))
     (R [#:pattern ?block]
        [#:pass1]
        [BlockPass ?block pass1]
        [#:pass2]
        [#:if (eq? trans 'letrec)
              (;; FIXME: foci (difficult because of renaming?)
               [#:walk (list (wderiv-e1 pass2)) 'block->letrec]
               [#:pattern (?expr)]
               [Expr ?expr pass2])
              ([#:rename ?block (wlderiv-es1 pass2)]
               [#:set-syntax (wlderiv-es1 pass2)]
               [List ?block pass2])])]
    [#f
     (R)]))

;; BlockPass : (list-of BRule) -> RST
(define (BlockPass brules)
  (match/count brules
    ['()
     (R)]
    [(cons (Wrap b:error (exn)) rest)
     (R [! exn])]
    [(cons (Wrap b:splice (renames head ?1 tail ?2)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [#:pass1]
        [Expr ?first head]
        [! ?1]
        [#:pass2]
        [#:let begin-form #'?first]
        [#:let rest-forms #'?rest]
        [#:pattern ?forms]
        [#:left-foot (list begin-form)]
        [#:set-syntax (append (stx->list (stx-cdr begin-form)) rest-forms)]
        [#:step 'splice-block (stx->list (stx-cdr begin-form))]
        [#:rename ?forms tail]
        [! ?2]
        [#:pattern ?forms]
        [BlockPass ?forms rest])]

    ;; FIXME: are these pass1/2 necessary?

    [(cons (Wrap b:defvals (renames head ?1 rename ?2)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [#:pass1]
        [Expr ?first head]
        [! ?1]
        [#:pattern ((?define-values ?vars . ?body) . ?rest)]
        [#:rename (?vars . ?body) rename]
        [#:binders #'?vars]
        [! ?2]
        [#:pass2]
        [#:pattern (?first . ?rest)]
        [BlockPass ?rest rest])]
    [(cons (Wrap b:defstx (renames head ?1 rename ?2 prep bindrhs)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [#:pass1]
        [Expr ?first head]
        [! ?1]
        [#:pattern ((?define-syntaxes ?vars . ?body) . ?rest)]
        [#:rename (?vars . ?body) rename]
        [#:binders #'?vars]
        [! ?2]
        [#:pass2]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern ((?define-syntaxes ?vars ?rhs) . ?rest)]
        [BindSyntaxes ?rhs bindrhs]
        [#:pattern (?first . ?rest)]
        [BlockPass ?rest rest])]
    [(cons (Wrap b:expr (renames head)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [BlockPass ?rest rest])]
    ))

;; BindSyntaxes : BindSyntaxes -> RST
(define (BindSyntaxes bindrhs)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs locals))
     (R [#:set-syntax (node-z1 rhs)] ;; set syntax; could be in local-bind
        [#:pattern ?form]
        [Expr/PhaseUp ?form rhs]
        [LocalActions ?form locals])]))

(define (BeginForSyntax passes)
  ;; Note: an lderiv doesn't necessarily cover all stxs, due to lifting.
  (match/count passes
    [(cons (? lderiv? lderiv) '())
     (R [#:pattern ?forms]
        [List ?forms lderiv])]
    [(cons (Wrap bfs:lift (lderiv stxs)) rest)
     (R [#:pattern LDERIV]
        [#:parameterize ((available-lift-stxs (reverse stxs))
                         (visible-lift-stxs null))
          [#:pass1]
          [List LDERIV lderiv]
          [#:do (when (pair? (available-lift-stxs))
                  (lift-error 'bfs:lift "available lifts left over"))]
          [#:let visible-lifts (visible-lift-stxs)]
          [#:pattern ?forms]
          [#:pass2]
          [#:let old-forms #'?forms]
          [#:left-foot null]
          [#:set-syntax (append visible-lifts old-forms)]
          [#:step 'splice-lifts visible-lifts]
          [#:set-syntax (append stxs old-forms)]
          [BeginForSyntax ?forms rest]])]))

(define (ModuleBegin/Phase body)
  (match/count body
    [(Wrap module-begin/phase (pass1 pass2 pass3))
     (R [#:pass1]
        [#:pattern ?forms]
        [ModulePass ?forms pass1]
        [#:pass2]
        [#:do (DEBUG (printf "** module begin pass 2\n"))]
        [ModulePass ?forms pass2]
        ;; ignore pass3 for now: only provides
        [#:new-local-context
         [#:pattern ?form]
         [LocalActions ?form (map expr->local-action (or pass3 null))]])]))

;; ModulePass : (list-of MBRule) -> RST
(define (ModulePass mbrules)
  (match/count mbrules
    ['()
     (R)]
    [(cons (Wrap mod:prim (head rename prim)) rest)
     (R [#:pattern (?firstP . ?rest)]
        [Expr ?firstP head]
        [#:do (DEBUG (printf "** after head\n"))]
        [#:rename ?firstP rename]
        [#:do (DEBUG (printf "** after rename\n"))]
        [#:when prim
                [Expr ?firstP prim]]
        [#:do (DEBUG (printf "** after prim\n"))]
        [ModulePass ?rest rest])]
    [(cons (Wrap mod:splice (head rename ?1 tail)) rest)
     (R [#:pattern (?firstB . ?rest)]
        [#:pass1]
        [Expr ?firstB head]
        [#:pass2]
        [#:rename ?firstB rename]
        [! ?1]
        [#:let begin-form #'?firstB]
        [#:let rest-forms #'?rest]
        [#:left-foot (list #'?firstB)]
        [#:pattern ?forms]
        [#:set-syntax (append (stx->list (stx-cdr begin-form)) rest-forms)]
        [#:step 'splice-module (stx->list (stx-cdr begin-form))]
        [#:rename ?forms tail]
        [ModulePass ?forms rest])]
    [(cons (Wrap mod:lift (head locals renames stxs)) rest)
     (R [#:pattern (?firstL . ?rest)]
        ;; renames has form (head-e2 . ?rest)
        ;; stxs has form (lifted ...),
        ;;   specifically (last-lifted ... first-lifted)
        [#:parameterize ((available-lift-stxs (reverse stxs))
                         (visible-lift-stxs null))
          [#:pass1]
          [Expr ?firstL head]
          [LocalActions ?firstL locals]
          [#:do (when (pair? (available-lift-stxs))
                  (lift-error 'mod:lift "available lifts left over"))]
          [#:let visible-lifts (visible-lift-stxs)]
          [#:pattern ?forms]
          [#:pass2]
          [#:when renames
                  [#:rename ?forms renames]]
          [#:let old-forms #'?forms]
          [#:left-foot null]
          [#:set-syntax (append visible-lifts old-forms)]
          [#:step 'splice-lifts visible-lifts]
          [#:set-syntax (append stxs old-forms)]
          [ModulePass ?forms rest]])]
    [(cons (Wrap mod:lift-end (stxs)) rest)
     ;; In pass2, stxs contains a mixture of terms and kind-tagged terms (pairs)
     (let ([stxs (map (lambda (e) (if (pair? e) (car e) e)) stxs)])
       (R [#:pattern ?forms]
          [#:when (pair? stxs)
                  [#:left-foot null]
                  [#:set-syntax (append stxs #'?forms)]
                  [#:step 'splice-module-lifts stxs]]
          [ModulePass ?forms rest]))]
    [(cons (Wrap mod:skip ()) rest)
     (R [#:pattern (?firstS . ?rest)]
        [ModulePass ?rest rest])]
    [(cons (Wrap mod:cons (head locals)) rest)
     (R [#:pattern (?firstC . ?rest)]
        [Expr ?firstC head]
        [LocalActions ?firstC locals]
        [ModulePass ?rest rest])]))

;; Lifts

(define (take-lift!)
  (define avail (available-lift-stxs))
  (cond [(list? avail)
         #|
         ;; This check is wrong! (and thus disabled)
         ;; If a syntax error occurs between the time a lift is "thrown"
         ;; and when it is "caught", no lifts will be available to take.
         ;; But that's not a bug, so don't complain.
         (unless (pair? avail)
           (lift-error 'local-lift "out of lifts (begin)!"))
         |#
         (when (pair? avail)
           (let ([lift-stx (car avail)])
             (available-lift-stxs (cdr avail))
             (when (visibility)
               (visible-lift-stxs
                (cons lift-stx (visible-lift-stxs))))))]
        [else
         (syntax-case avail ()
           [(?let-values ?lift ?rest)
            (eq? (syntax-e #'?let-values) 'let-values)
            (begin (available-lift-stxs #'?rest)
                   (when (visibility)
                     (visible-lift-stxs
                      (cons (datum->syntax avail (list #'?let-values #'?lift)
                                           avail avail)
                            (visible-lift-stxs)))))]
           [_
            (lift-error 'local-lift "out of lifts (let)!")])]))

(define (reform-begin-lifts orig-lifted lifts body)
  (define begin-kw (stx-car orig-lifted))
  (datum->syntax orig-lifted
                 `(,begin-kw ,@lifts ,body)
                 orig-lifted
                 orig-lifted))

(define (reform-let-lifts orig-lifted lifts body)
  (if (null? lifts)
      body
      (reform-let-lifts orig-lifted
                        (cdr lifts)
                        (with-syntax ([(?let-values ?lift) (car lifts)])
                          (datum->syntax (car lifts)
                                         `(,#'?let-values ,#'?lift ,body)
                                         (car lifts)
                                         (car lifts))))))

;; lift-error
(define (lift-error sym . args)
  (apply eprintf args)
  (newline (current-error-port))
  (when #f
    (apply error sym args)))

(define (expr->local-action d)
  (match d
    [(Wrap deriv (e1 e2))
     (make local-expansion e1 e2
           #f e1 d #f e2 #f)]))

;; opaque-table
;; Weakly remembers assoc between opaque values and
;; actual syntax, so that actual can be substituted in
;; for destructuring.
;; FIXME: perhaps add event for opaque-stx unwrapping?
(define opaque-table (make-weak-hasheq))
