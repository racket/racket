
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
(define ((CaseLambdaClauses e1))
  (mk-case-lambda-clauses-reductions e1))
(define ((SynthItems e1))
  (mk-synth-items-reductions e1))
(define ((BRules es1))
  (mk-brules-reductions es1))
(define ((ModulePass es1))
  (mk-mbrules-reductions es1))

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
  (parameterize ((current-definites null)
                 (current-frontier null))
    (when d (add-frontier (list (wderiv-e1 d))))
    (RS-steps (reductions* d))))

;; reductions+ : WDeriv -> (list-of step) (list-of identifier) ?stx ?exn
(define (reductions+ d)
  (parameterize ((current-definites null)
                 (current-frontier null))
    (when d (add-frontier (list (wderiv-e1 d))))
    (let-values ([(rs stx exn) (reductions* d)])
      (values rs (current-definites) stx exn))))

;; reductions* : WDeriv -> RS(stx)
(define (reductions* d)
  (match d
    [(Wrap deriv (e1 e2))
     (blaze-frontier e1)]
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
                   [#:walk e2 e1 e2 'resolve-variable]])]
    [(Wrap p:module (e1 e2 rs ?1 #f #f #f body))
     (R e1
        [! ?1]
        [#:pattern (?module ?name ?language . ?_body)]
        [#:walk (d->so e1 `(,#'?module ,#'?name ,#'?language ,(wderiv-e1 body)))
                'tag-module-begin]
        [#:pattern (?module ?name ?language ?body)]
        [#:frontier (list #'?body)]
        [Expr ?body body])]
    [(Wrap p:module (e1 e2 rs ?1 #t mb ?2 body))
     (R e1
        [! ?1]
        [#:pattern (?module ?name ?language ?body)]
        [#:frontier (list #'?body)]
        [Expr ?body mb]
        [! ?2]
        [#:when/np (not (eq? (wderiv-e2 mb) (wderiv-e1 body)))
                   [#:walk
                    (d->so e1 `(,#'?module ,#'?name ,#'?language
                                           ,(wderiv-e1 body)))
                    'tag-module-begin]]
        [Expr ?body body])]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 pass1 pass2 ?2))
     (R e1
        [! ?1]
        [#:pattern (?module-begin . ?forms)]
        [#:frontier (stx->list* #'?forms)]
        [(ModulePass #'?forms)
         ?forms pass1]
        [(ModulePass #'?forms)
         ?forms pass2]
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
    [(Wrap p:if (e1 e2 rs ?1 full? test then else))
     (if full?
         (R e1
            [! ?1]
            [#:pattern (?if TEST THEN ELSE)]
            [#:frontier (list #'TEST #'THEN #'ELSE)]
            [Expr TEST test]
            [Expr THEN then]
            [Expr ELSE else])
         (R e1
            [! ?1]
            [#:pattern (?if TEST THEN)]
            [#:frontier (list #'TEST #'THEN)]
            [Expr TEST test]
            [Expr THEN then]))]
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
    [(Wrap p:#%app (e1 e2 rs ?1 tagged-stx lderiv))
     (R e1
        [! ?1]
        [#:when/np (not (eq? tagged-stx e1))
                   [#:walk tagged-stx 'tag-app]]
        [#:pattern (?app . LDERIV)]
        [#:frontier (stx->list* #'LDERIV)]
        [List LDERIV lderiv])]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (R e1
        [! ?1]
        [#:bind (?formals* . ?body*) renames]
        [#:pattern (?lambda ?formals . ?body)]
        [#:frontier (stx->list* #'?body)]
        [#:rename (syntax/skeleton e1 (?lambda ?formals* . ?body*))
                  #'?formals #'?formals*
                  'rename-lambda]
        [Block ?body body])]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (R e1
        [! ?1]
        [#:pattern (?case-lambda . ?clauses)]
        [#:frontier (stx->list* #'?clauses)]
        [(CaseLambdaClauses (stx->list* #'?clauses))
         ?clauses clauses])]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (R e1
        [! ?1]
        [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
        [#:frontier (append (syntax->list #'(?rhs ...)) (stx->list* #'?body))]
        [#:bind (([?vars* ?rhs*] ...) . ?body*) renames]
        [#:rename
         (syntax/skeleton e1 (?let-values ([?vars* ?rhs*] ...) . ?body*))
         (syntax->list #'(?vars ...))
         (syntax->list #'(?vars* ...))
         'rename-let-values]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (R e1
        [! ?1]
        [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
        [#:frontier (append (syntax->list #'(?rhs ...)) (stx->list* #'?body))]
        [#:bind (([?vars* ?rhs*] ...) . ?body*) renames]
        [#:rename
         (syntax/skeleton e1 (?letrec-values ([?vars* ?rhs*] ...) . ?body*))
         (syntax->list #'(?vars ...))
         (syntax->list #'(?vars* ...))
         'rename-letrec-values]
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
        [#:bind (([?svars* ?srhs*] ...) ([?vvars* ?vrhs*] ...) . ?body*) srenames]
        [#:rename
         (syntax/skeleton e1
                          (?lsv ([?svars* ?srhs*] ...) ([?vvars* ?vrhs*] ...)
                            . ?body*))
         (syntax->list #'(?svars ...))
         (syntax->list #'(?svars* ...))
         'rename-lsv]
        [BindSyntaxes (?srhs ...) srhss]
        ;; If vrenames is #f, no var bindings to rename
        [#:when/np vrenames
                   [#:bind (([?vvars** ?vrhs**] ...) . ?body**) vrenames]
                   [#:rename
                    (syntax/skeleton e1 (?lsv ([?svars* ?srhs*] ...)
                                              ([?vvars** ?vrhs**] ...)
                                              . ?body**))
                    (syntax->list #'(?vvars* ...))
                    (syntax->list #'(?vvars** ...))
                    'rename-lsv]]
        [Expr (?vrhs ...) vrhss]
        [Block ?body body]
        [#:pattern ?form]
        [#:when/np (not (eq? #'?form e2)) ;; FIXME: correct comparison?
                   [#:walk e2 'lsv-remove-syntax]])]
    ;; The auto-tagged atomic primitives
    [(Wrap p:#%datum (e1 e2 rs ?1 tagged-stx))
     (R e1
        [#:when/np (not (eq? e1 tagged-stx))
                   [#:walk tagged-stx 'tag-datum]]
        [! ?1])]
    [(Wrap p:#%top (e1 e2 rs ?1 tagged-stx))
     (R e1
        [#:when/np (not (eq? e1 tagged-stx))
                   [#:walk tagged-stx 'tag-top]]
        [#:pattern (?top . ?var)]
        [#:learn (list #'?var)]
        [! ?1])]
    
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
        [(SynthItems e1) ?form subterms]
        [! ?2])]

    ;; FIXME: elimiate => ??
    [(Wrap p:rename (e1 e2 rs ?1 rename inner))
     (R e1
        [! ?1]
        =>
        (lambda (e)
          (rename-frontier (car rename) (cdr rename))
          (reductions* inner)))]
    
    ;; Macros
    [(Wrap mrule (e1 e2 transformation next))
     (R e1
        [#:pattern ?form]
        [Transformation ?form transformation]
        [#:frontier (list (wderiv-e1 next))]
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
    [#f (RSzero)]))

;; mk-case-lambda-clauses-reductions : stxs ->
;;   (list-of (W (list ?exn rename (W BDeriv)))) -> (RS stxs)
(define ((mk-case-lambda-clauses-reductions es1) clauses)
  (blaze-frontier es1)
  (match clauses
    ['()
     (RSunit null)]
    [(cons (Wrap clc (?1 rename body)) rest)
     (R es1
        [! ?1]
        [#:pattern ((?formals . ?body) . ?rest)]
        [#:frontier (list #'?body #'?rest)]
        [#:bind (?formals* . ?body*) rename]
        [#:rename (syntax/skeleton es1 ((?formals* . ?body*) . ?rest))
                  #'?formals #'?formals*
                  'rename-case-lambda]
        [Block ?body body]
        [(CaseLambdaClauses (cdr es1))
         ?rest rest])]))

;; mk-synth-items-reductions : syntax -> (list-of SynthItem) -> (RS syntax)
(define ((mk-synth-items-reductions e1) subterms)
  (let loop ([term e1] [subterms subterms])
    (cond [(null? subterms)
           (RSunit e1)]
          [(s:subterm? (car subterms))
           (let* ([subterm0 (car subterms)]
                  [path0 (s:subterm-path subterm0)]
                  [deriv0 (s:subterm-deriv subterm0)])
             (let ([ctx (lambda (x) (path-replace term path0 x))])
               (RSseq (lambda ()
                        (with-context ctx (reductions* deriv0)))
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

;; transformation-reductions : Transformation -> (RS Stx)
(define (transformation-reductions tx)
  (match tx
    [(Wrap transformation (e1 e2 rs ?1 me1 locals ?2 me2 seq))
     (R e1
        [! ?1]
        [#:pattern ?form]
        [#:learn rs]
        [#:reductions (reductions-locals e1 locals)]
        [! ?2]
        [#:walk e2
                (list #'?form)
                (list e2)
                'macro])]))

;; reductions-locals : syntax (list-of LocalAction) -> (RS void)
(define (reductions-locals stx locals)
  (with-new-local-context stx
    (RSforeach reductions-local locals)))

;; reductions-local : LocalAction -> (RS void)
(define (reductions-local local)
  (match/with-derivation local
    [(struct local-expansion (e1 e2 me1 me2 for-stx? deriv))
     (reductions* deriv)]
    [(struct local-expansion/expr (e1 e2 me1 me2 for-stx? opaque deriv))
     (fprintf (current-error-port)
              "reductions: local-expand-expr not fully implemented")
     (reductions* deriv)]
    [(struct local-lift (expr id))
     (RSadd (list (walk expr id 'local-lift))
            RSzero)]
    [(struct local-lift-end (decl))
     (RSadd (list (walk/mono decl 'module-lift))
            RSzero)]
    [(struct local-bind (bindrhs))
     (bind-syntaxes-reductions bindrhs)]))

;; list-reductions : ListDerivation -> (RS Stxs)
(define (list-reductions ld)
  (match/with-derivation ld
    [(Wrap lderiv (es1 es2 ?1 derivs))
     (R es1
        [! ?1]
        [#:pattern (?form ...)]
        [Expr (?form ...) derivs])]
    [#f (RSunit null)]))

;; block-reductions : BlockDerivation -> (RS Stxs)
(define (block-reductions bd)
  (match/with-derivation bd
    [(Wrap bderiv (es1 es2 pass1 trans pass2))
     (R es1
        [#:pattern ?form]
        [(BRules es1) ?form pass1]
        [#:when/np (eq? trans 'letrec)
                   [#:walk (wlderiv-es1 pass2) 'block->letrec]]
        [#:frontier (stx->list* (wlderiv-es1 pass2))]
        [#:pattern ?form]
        [List ?form pass2])]
    [#f (RSunit null)]))

;; mk-brules-reductions : stxs -> (list-of BRule) -> (RS Stxs)
(define ((mk-brules-reductions es1) brules)
  (match brules
    ['()
     (RSunit null)]
    [(cons (Wrap b:expr (renames head)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [(BRules (stx-cdr es1)) ?rest rest])]
    [(cons (Wrap b:defvals (renames head ?1)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [! ?1]
        [#:pattern ((?define-values ?vars ?rhs) . ?rest)]
        [#:learn (syntax->list #'?vars)]
        [(BRules (stx-cdr es1)) ?rest rest])]
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
        [(BRules (stx-cdr es1)) ?rest rest])]
    [(cons (Wrap b:splice (renames head ?1 tail ?2)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [#:bind ?first* (cdr renames)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [! ?1]
        [#:walk tail
                (list #'?first)
                (stx-take tail (- (stx-improper-length tail)
                                  (stx-improper-length #'?rest)))
                'splice-block]
        [! ?2]
        [#:pattern ?forms]
        [(BRules (stx->list* #'?forms)) ?forms rest])]
    [(cons (Wrap b:error (exn)) rest)
     (R es1
        [! exn])]))

;; bind-syntaxes-reductions : BindSyntaxes -> (RS stx)
(define (bind-syntaxes-reductions bindrhs)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs ?1))
     (R (wderiv-e1 rhs)
        [#:pattern ?form]
        [Expr ?form rhs]
        [! ?1])]))

;; mk-mbrules-reductions : stx -> (list-of MBRule) -> (RS stxs)
(define ((mk-mbrules-reductions es1) mbrules)
  (match mbrules
    ['()
     (RSunit null)]
    [(cons (Wrap mod:skip ()) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [(ModulePass (stx-cdr es1)) ?rest rest])]
    [(cons (Wrap mod:cons (head)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [(ModulePass (stx-cdr es1)) ?rest rest])]
    [(cons (Wrap mod:prim (head prim)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [Expr ?first prim]
        [(ModulePass (stx-cdr es1)) ?rest rest])]
    [(cons (Wrap mod:splice (head ?1 tail)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [! ?1]
        [#:walk tail
                (list #'?first)
                (stx-take tail (- (stx-improper-length tail)
                                  (stx-improper-length #'?rest)))
                'splice-module]
        [#:pattern ?forms]
        [(ModulePass #'?forms) ?forms rest])]
    [(cons (Wrap mod:lift (head stxs)) rest)
     (R es1
        [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [#:pattern ?forms]
        [#:walk (append stxs #'?forms)
                null
                stxs
                'splice-lifts]
        [(ModulePass #'?forms) ?forms rest])]
    [(cons (Wrap mod:lift-end (stxs)) rest)
     (R es1
        [#:pattern ?forms]
        [#:when/np (pair? stxs)
                   [#:walk (append stxs #'?forms)
                           null
                           stxs
                           'splice-module-lifts]]
        [(ModulePass #'?forms) ?forms rest])]))
