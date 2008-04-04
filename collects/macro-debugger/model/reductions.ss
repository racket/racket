
#lang scheme/base
(require scheme/match
         "stx-util.ss"
         "deriv-util.ss"
         "context.ss"
         "deriv.ss"
         "reductions-engine.ss")

(provide reductions
         reductions+)

;; Setup for reduction-engines

(define (Expr) reductions*)
(define (List) list-reductions)
(define (Block) block-reductions)
(define (Transformation)
  transformation-reductions)
(define (BindSyntaxes)
  bind-syntaxes-reductions)
(define (CaseLambdaClauses)
  case-lambda-clauses-reductions)
(define (SynthItems)
  synth-items-reductions)
(define (BRules)
  brules-reductions)
(define (ModulePass)
  mbrules-reductions)

;; Syntax

(define-syntax match/with-derivation
  (syntax-rules ()
    [(match/with-derivation d . clauses)
     (let ([dvar d])
       (with-derivation dvar
         (match dvar . clauses)))]))

;; Reductions

;; reductions : WDeriv -> ReductionSequence
(define (reductions d)
  (let-values ([(steps definites estx exn) (reductions+ d)])
    steps))

;; reductions+ : WDeriv -> (list-of step) (list-of identifier) ?stx ?exn
(define (reductions+ d)
  (parameterize ((current-definites null)
                 (current-frontier null))
    (when d (add-frontier (list (wderiv-e1 d))))
    (let-values ([(steps stx exn) (reductions* d (wderiv-e1 d))])
      (values steps (current-definites) stx exn))))

;; reductions* : WDeriv Syntax -> RS(stx)
(define (reductions* d init-e1)
  (match d
    [(Wrap deriv (e1 e2))
     (begin (blaze-frontier e1)
            (unless (eq? init-e1 e1)
              (void)
              #;(fprintf (current-error-port)
                       "starting points don't match:\n~s\n~s\n"
                       init-e1 e1)
              #;(error 'reductions* "starting points don't match for: ~s" d)))]
    [_ (void)])
  (match d
    [(Wrap prule (e1 e2 rs ?1))
     (and rs (learn-definites rs))]
    [_ (void)])
  (match/with-derivation d
    ;; Primitives
    [(Wrap p:variable (e1 e2 rs ?1))
     (R e1
        [#:learn (list e2)]
        [#:when/np (not (bound-identifier=? e1 e2))
                   [#:walk e2 'resolve-variable]])]
    [(Wrap p:module (e1 e2 rs ?1 ?2 tag rename check tag2 ?3 body shift))
     (R e1
        [! ?1]
        [#:pattern (?module ?name ?language . ?body-parts)]
        #;[#:frontier null (list #'?language #'?body-parts)]
        [! ?2]
        #;[#:frontier (list #'?language) null]
        [#:when/np tag
                   [#:walk/ctx ?body-parts
                               (list tag)
                               'tag-module-begin]]
        [#:pattern (?module ?name ?language ?body)]
        [#:rename* ?body rename]
        [#:when/np check
                   [Expr ?body check]]
        [#:when/np tag2
                   [#:walk/ctx ?body
                               tag2
                               'tag-module-begin]]
        [! ?3]
        [Expr ?body body]
        [#:pattern ?form]
        [#:rename* ?form shift])]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 me pass1 pass2 ?2))
     (R e1
        [! ?1]
        #;[#:let-values (_) (printf "#%module-begin:\n~s\n" me)]
        [#:pattern ?form]
        [#:rename* ?form me]
        [#:pattern (?module-begin . ?forms)]
        #;[#:frontier (syntax->list #'?forms)]
        #;[#:let-values (_) (printf "#%module-begin ?forms:\n~s\n" #'?forms)]
        [ModulePass ?forms pass1]
        [ModulePass ?forms pass2]
        [! ?1])]
    [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs ?2))
     (R e1
        [! ?1]
        [#:pattern (?define-syntaxes formals ?rhs)]
        [#:frontier (list #'?rhs)]
        [Expr ?rhs rhs]
        [! ?2])]
    [(Wrap p:define-values (e1 e2 rs ?1 rhs))
     (R e1
        [! ?1]
        [#:pattern (?define-values ?formals ?rhs)]
        [#:frontier (list #'?rhs)]
        ;; RHS can be #f (eg, modprim)
        [#:when/np rhs
                   [Expr ?rhs rhs]])]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner))
     (R e1
        [! ?1]
        [#:pattern (?expr ?inner)]
        [#:frontier (list #'?inner)]
        [Expr ?inner inner])]
    [(Wrap p:if (e1 e2 rs ?1 test then else))
     (R e1
        [! ?1]
        [#:pattern (?if TEST THEN ELSE)]
        [#:frontier (list #'TEST #'THEN #'ELSE)]
        [Expr TEST test]
        [Expr THEN then]
        [Expr ELSE else])]
    [(Wrap p:wcm (e1 e2 rs ?1 key mark body))
     (R e1
        [! ?1]
        [#:pattern (?wcm KEY MARK BODY)]
        [#:frontier (list #'KEY #'MARK #'BODY)]
        [Expr KEY key]
        [Expr MARK mark]
        [Expr BODY body])]
    [(Wrap p:begin (e1 e2 rs ?1 lderiv))
     (R e1
        [! ?1]
        [#:pattern (?begin . ?lderiv)]
        [#:frontier (stx->list* #'?lderiv)]
        [List ?lderiv lderiv])]
    [(Wrap p:begin0 (e1 e2 rs ?1 first lderiv))
     (R e1
        [! ?1]
        [#:pattern (?begin0 FIRST . LDERIV)]
        [#:frontier (cons #'FIRST (stx->list* #'LDERIV))]
        [Expr FIRST first]
        [List LDERIV lderiv])]
    [(Wrap p:#%app (e1 e2 rs ?1 lderiv))
     (R e1
        [! ?1]
        [#:pattern (?app . LDERIV)]
        [#:frontier (stx->list* #'LDERIV)]
        [List LDERIV lderiv])]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (R e1
        [! ?1]
        [#:pattern (?lambda ?formals . ?body)]
        [#:frontier (stx->list* #'?body)]
        [#:rename* (?formals . ?body) renames 'rename-lambda]
        [Block ?body body])]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (R e1
        [! ?1]
        [#:pattern (?case-lambda . ?clauses)]
        [#:frontier (stx->list* #'?clauses)]
        [CaseLambdaClauses ?clauses clauses])]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (R e1
        [! ?1]
        [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
        [#:frontier (append (syntax->list #'(?rhs ...)) (stx->list* #'?body))]
        [#:rename* (((?vars ?rhs) ...) . ?body) renames 'rename-let-values]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (R e1
        [! ?1]
        [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
        [#:frontier (append (syntax->list #'(?rhs ...)) (stx->list* #'?body))]
        [#:rename* (((?vars ?rhs) ...) . ?body) renames 'rename-letrec-values]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-syntaxes+values
           (e1 e2 rs ?1 srenames srhss vrenames vrhss body))
     (R e1
        [! ?1]
        [#:pattern (?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body)]
        [#:frontier (append (syntax->list #'(?srhs ...))
                            (syntax->list #'(?vrhs ...))
                            (stx->list* #'?body))]
        [#:rename* (((?svars ?srhs) ...) ((?vvars ?vrhs) ...) . ?body) srenames
                   'rename-lsv]
        [BindSyntaxes (?srhs ...) srhss]
        ;; If vrenames is #f, no var bindings to rename
        [#:when/np vrenames
                   [#:bind (([?vvars** ?vrhs**] ...) . ?body**) vrenames]
                   [#:rename* (((?vars ?vrhs) ...) . ?body) vrenames 'rename-lsv]]
        [Expr (?vrhs ...) vrhss]
        [Block ?body body]
        [#:pattern ?form]
        [#:when/np (not (eq? #'?form e2)) ;; FIXME: correct comparison?
                   [#:walk e2 'lsv-remove-syntax]])]

    [(Wrap p:#%datum (e1 e2 rs ?1))
     (R e1
        [! ?1]
        [#:walk e2 'macro])]
    [(Wrap p:#%top (e1 e2 rs ?1))
     (R e1
        [#:pattern (?top . ?var)]
        [#:learn (list #'?var)]
        [! ?1])]

    [(Wrap p:provide (e1 e2 rs ?1))
     (R e1
        [! ?1]
        [#:walk e2 'provide])]

    ;; The rest of the automatic primitives
    [(Wrap p::STOP (e1 e2 rs ?1))
     (R e1
        [! ?1])]

    [(Wrap p:set!-macro (e1 e2 rs ?1 deriv))
     (R e1
        [! ?1]
        [#:frontier (list e1)]
        [#:pattern ?form]
        [Expr ?form deriv])]
    [(Wrap p:set! (e1 e2 rs ?1 id-rs rhs))
     (R e1
        [! ?1]
        [#:pattern (?set! ?var ?rhs)]
        [#:frontier (list #'?rhs)]
        [#:learn id-rs]
        [Expr ?rhs rhs])]

    ;; Synthetic primitives
    ;; These have their own subterm replacement mechanisms
    [(Wrap p:synth (e1 e2 rs ?1 subterms ?2))
     (R e1
        [! ?1]
        [#:pattern ?form]
        [#:frontier
         ;; Compute the frontier based on the expanded subterms
         ;; Run through the renames in reverse order to get the
         ;; pre-renamed terms
         (parameterize ((current-frontier null))
           (let loop ([subterms subterms])
             (cond [(null? subterms)
                    (void)]
                   [(s:subterm? (car subterms))
                    (loop (cdr subterms))
                    (add-frontier
                     (list (wderiv-e1 (s:subterm-deriv (car subterms)))))]
                   [(s:rename? (car subterms))
                    (loop (cdr subterms))
                    (rename-frontier (s:rename-after (car subterms))
                                     (s:rename-before (car subterms)))]))
           (current-frontier))]
        [SynthItems ?form subterms]
        [! ?2])]

    ;; FIXME: elimiate => ??
    [(Wrap p:rename (e1 e2 rs ?1 rename inner))
     (R e1
        [! ?1]
        [#:pattern ?form]
        =>
        (lambda (e)
          (rename-frontier (car rename) (cdr rename))
          (reductions* inner (wderiv-e1 inner))))]
    
    ;; Macros
    [(Wrap mrule (e1 e2 transformation next))
     (R e1
        [#:pattern ?form]
        [Transformation ?form transformation]
        [#:frontier (list (wderiv-e1 next))]
        [Expr ?form next])]

    [(Wrap tagrule (e1 e2 tagged-stx next))
     (R e1
        [#:pattern ?form]
        [#:walk tagged-stx
                (case (syntax-e (stx-car tagged-stx))
                  ((#%app) 'tag-app)
                  ((#%datum) 'tag-datum)
                  ((#%top) 'tag-top)
                  (else
                   (error 'reductions "unknown tagged syntax: ~s" tagged-stx)))]
        [Expr ?form next])]

    ;; Lifts

    [(Wrap lift-deriv (e1 e2 first lifted-stx second))
     (R e1
        [#:pattern ?form]
        [Expr ?form first]
        [#:frontier (list lifted-stx)]
        [#:walk lifted-stx 'capture-lifts]
        [Expr ?form second])]

    [(Wrap lift/let-deriv (e1 e2 first lifted-stx second))
     (R e1
        [#:pattern ?form]
        [Expr ?form first]
        [#:frontier (list lifted-stx)]
        [#:walk lifted-stx 'capture-lifts]
        [Expr ?form second])]

    ;; Skipped
    [#f (RSunit init-e1)]))

;; case-lambda-clauses-reductions : 
;;   (list-of (W (list ?exn rename (W BDeriv)))) stxs -> (RS stxs)
(define (case-lambda-clauses-reductions clauses es1)
  (blaze-frontier es1)
  (match clauses
    ['()
     (RSunit null)]
    [(cons (Wrap clc (?1 rename body)) rest)
     (R es1
        [! ?1]
        [#:pattern ((?formals . ?body) . ?rest)]
        [#:frontier (list #'?body #'?rest)]
        [#:rename* (?formals . ?body) rename 'rename-case-lambda]
        [Block ?body body]
        [CaseLambdaClauses ?rest rest])]))

;; synth-items-reductions : (list-of SynthItem) syntax -> (RS syntax)
(define (synth-items-reductions subterms e1)
  (let loop ([term e1] [subterms subterms])
    (cond [(null? subterms)
           (RSunit e1)]
          [(s:subterm? (car subterms))
           (let* ([subterm0 (car subterms)]
                  [path0 (s:subterm-path subterm0)]
                  [deriv0 (s:subterm-deriv subterm0)])
             (let ([ctx (lambda (x) (path-replace term path0 x))]
                   ;; unused: may not be the same, due to mark/unmark???
                   [init-e (path-get term path0)])
               (RSseq (lambda ()
                        (with-context ctx
                          (reductions* deriv0 (wderiv-e1 deriv0))))
                      (lambda ()
                        (loop (path-replace term path0 (wderiv-e2 deriv0))
                              (cdr subterms))))))]
          [(s:rename? (car subterms))
           (let* ([subterm0 (car subterms)])
             ;; FIXME: add renaming steps?
             ;; FIXME: if so, coalesce?
             (rename-frontier (s:rename-before subterm0)
                              (s:rename-after subterm0))
             (loop (path-replace term
                                 (s:rename-path subterm0)
                                 (s:rename-after subterm0))
                   (cdr subterms)))])))

;; transformation-reductions : Transformation stx -> (RS Stx)
(define (transformation-reductions tx init-e1)
  (match tx
    [(Wrap transformation (e1 e2 rs ?1 me1 locals me2 ?2 seq))
     (R e1
        [! ?1]
        [#:pattern ?form]
        [#:learn rs]
        [#:reductions (reductions-locals e1 locals)]
        [! ?2]
        [#:walk e2 'macro])]))

;; reductions-locals : syntax (list-of LocalAction) -> (RS void)
(define (reductions-locals stx locals)
  (with-new-local-context stx
    (RSforeach reductions-local locals)))

;; reductions-local : LocalAction -> (RS void)
(define (reductions-local local)
  (match/with-derivation local
    [(struct local-expansion (e1 e2 me1 me2 deriv for-stx? lifted opaque))
     ;; FIXME
     ;; When lifted is present, need to locally rearrange lifts!
     (when (or lifted opaque)
       (fprintf (current-error-port)
                "reductions: local-expand-expr not fully implemented"))
     (reductions* deriv me1)]
    [(struct local-lift (expr id))
     (RSadd (list (walk expr id 'local-lift))
            RSzero)]
    [(struct local-lift-end (decl))
     (RSadd (list (walk/mono decl 'module-lift))
            RSzero)]
    [(struct local-bind (names bindrhs))
     (bind-syntaxes-reductions bindrhs)]))

;; list-reductions : ListDerivation stxs -> (RS Stxs)
(define (list-reductions ld init-es1)
  (match/with-derivation ld
    [(Wrap lderiv (es1 es2 ?1 derivs))
     (R es1
        [! ?1]
        [#:pattern (?form ...)]
        [Expr (?form ...) derivs])]
    [#f (RSunit null)]))

;; block-reductions : BlockDerivation stxs -> (RS Stxs)
(define (block-reductions bd init-es1)
  (match/with-derivation bd
    [(Wrap bderiv (es1 es2 pass1 trans pass2))
     (R es1
        [#:pattern ?form]
        [BRules ?form pass1]
        [#:when/np (eq? trans 'letrec)
                   [#:walk (wlderiv-es1 pass2) 'block->letrec]]
        [#:frontier (stx->list* (wlderiv-es1 pass2))]
        [#:pattern ?form]
        [List ?form pass2])]
    [#f (RSunit null)]))

;; brules-reductions : (list-of BRule) stxs -> (RS Stxs)
(define (brules-reductions brules es1)
  (match brules
    ['()
     (RSunit null)]
    [(cons (Wrap b:expr (renames head)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [BRules ?rest rest])]
    [(cons (Wrap b:defvals (renames head ?1)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [! ?1]
        [#:pattern ((?define-values ?vars ?rhs) . ?rest)]
        [#:learn (syntax->list #'?vars)]
        [BRules ?rest rest])]
    [(cons (Wrap b:defstx (renames head ?1 bindrhs)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [! ?1]
        [#:pattern ((?define-syntaxes ?vars ?rhs) . ?rest)]
        [#:learn (syntax->list #'?vars)]
        [BindSyntaxes ?rhs bindrhs]
        [BRules ?rest rest])]
    [(cons (Wrap b:splice (renames head ?1 tail ?2)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [! ?1]
        [#:walk/foci tail
                     (list #'?first)
                     (stx-take tail (- (stx-improper-length tail)
                                       (stx-improper-length #'?rest)))
                     'splice-block]
        [! ?2]
        [#:pattern ?forms]
        [BRules ?forms rest])]
    [(cons (Wrap b:error (exn)) rest)
     (R es1
        [! exn])]))

;; bind-syntaxes-reductions : BindSyntaxes stx -> (RS stx)
(define (bind-syntaxes-reductions bindrhs init-e1)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs ?1))
     (R (wderiv-e1 rhs)
        [#:pattern ?form]
        [Expr ?form rhs]
        [! ?1])]))

;; mbrules-reductions : -> (list-of MBRule) stxs -> (RS stxs)
(define (mbrules-reductions mbrules es1)
  (match mbrules
    ['()
     (RSunit null)]
    [(cons (Wrap mod:prim (head rename prim)) rest)
     (R es1
        [#:pattern (?firstP . ?rest)]
        [Expr ?firstP head]
        [#:rename* ?firstP rename]
        [Expr ?firstP prim]
        [ModulePass ?rest rest])]
    [(cons (Wrap mod:splice (head rename ?1 tail)) rest)
     (R es1
        [#:pattern (?firstB . ?rest)]
        [Expr ?firstB head]
        [#:rename* ?firstB rename]
        [! ?1]
        [#:walk/foci tail
                     (list #'?firstB)
                     (stx-take tail (- (stx-improper-length tail)
                                       (stx-improper-length #'?rest)))
                     'splice-module]
        [#:pattern ?forms]
        [ModulePass ?forms rest])]
    [(cons (Wrap mod:lift (head renames stxs)) rest)
     (R es1
        [#:pattern (?firstL . ?rest)]
        [Expr ?firstL head]
        [#:pattern ?forms]
        [#:when/np renames
                   [#:rename* ?forms renames]]
        [#:walk/foci (append stxs #'?forms)
                     null
                     stxs
                     'splice-lifts]
        [ModulePass ?forms rest])]
    [(cons (Wrap mod:lift-end (stxs)) rest)
     (R es1
        [#:pattern ?forms]
        [#:when/np (pair? stxs)
                   [#:walk/foci (append stxs #'?forms)
                                null
                                stxs
                                'splice-module-lifts]]
        [ModulePass ?forms rest])]
    [(cons (Wrap mod:skip ()) rest)
     (R es1
        [#:pattern (?firstS . ?rest)]
        [ModulePass ?rest rest])]
    [(cons (Wrap mod:cons (head)) rest)
     (R es1
        [#:pattern (?firstC . ?rest)]
        [Expr ?firstC head]
        [ModulePass ?rest rest])]))
