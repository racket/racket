#lang racket/base
(require (for-syntax racket/base)
         syntax/stx
         "yacc-ext.rkt"
         "yacc-interrupted.rkt"
         "deriv.rkt"
         "deriv-util.rkt"
         "deriv-tokens.rkt")
(provide parse-derivation)

(define (deriv-error ok? name value start end)
  (if ok?
      (error 'derivation-parser
             "error on token #~a: <~s, ~s>"
             start name value)
      (error 'derivation-parser "bad token #~a" start)))

;; PARSER

(define-production-splitter production/I values values)

(define-syntax (productions/I stx)
  (syntax-case stx ()
    [(productions/I def ...)
     #'(begin (production/I def) ...)]))

(define parse-derivation
  (parser
   (options (start Expansion)
            (src-pos)
            (tokens basic-empty-tokens basic-tokens prim-tokens renames-tokens)
            (end EOF)
            #| (debug "/tmp/DEBUG-PARSER.txt") |#
            (error deriv-error))

   ;; tokens
   (skipped-token-values
    visit resolve next next-group return
    enter-macro macro-pre-transform macro-post-transform exit-macro 
    enter-prim exit-prim
    enter-block block->list block->letrec splice
    enter-list exit-list
    enter-check exit-check
    local-post exit-local exit-local/expr
    local-bind enter-bind exit-bind exit-local-bind
    local-value-result local-value-binding
    phase-up module-body
    renames-lambda
    renames-case-lambda
    renames-let
    renames-letrec-syntaxes
    renames-block
    rename-one
    rename-list
    tag
    IMPOSSIBLE
    start
    top-non-begin
    prepare-env)

    ;; Entry point
   (productions
    (Expansion
     [(start EE/Lifts) $2]
     [(start EE/Lifts/Interrupted) $2]
     [(start ExpandCTE) $2]
     [(start ExpandCTE/Interrupted) $2]))

   (productions/I

    (ExpandCTE
     ;; The first 'Eval' is there for---I believe---lazy phase 1 initialization.
     [(visit start (? Eval) (? CheckImmediateMacro/Lifts)
             top-non-begin start (? EE) (? Eval) return)
      (make ecte $1 $9 $3 $4 $7 $8)]
     [(visit start Eval CheckImmediateMacro/Lifts
             top-begin (? NextExpandCTEs) return)
      (begin
        (unless (list? $6)
          (error "NextExpandCTEs returned non-list ~s" $6))
        (make ecte $1 $7 $3 $4
              (make p:begin $5 $7 (list (stx-car $5)) #f
                    (make lderiv (cdr (stx->list $5))
                          (and $7 (cdr (stx->list $7)))
                          #f
                          $6))
              null))])

    (CheckImmediateMacro/Lifts
     [((? CheckImmediateMacro))
      $1]
     [(CheckImmediateMacro lift-loop)
      (let ([e1 (wderiv-e1 $1)]
            [e2 $2])
        (make lift-deriv e1 e2 $1 $2 (make p:stop $2 $2 null #f)))])

    (NextExpandCTEs
     (#:skipped null)
     [() null]
     [(next (? ExpandCTE) (? NextExpandCTEs)) (cons $2 $3)])

    ;; Expand with possible lifting
    (EE/Lifts
     [((? EE)) $1]
     [(EE lift-loop (? EE/Lifts))
      (let ([e1 (wderiv-e1 $1)]
            [e2 (wderiv-e2 $3)])
        (make lift-deriv e1 e2 $1 $2 $3))])

    ;; Expand, convert lifts to let (rhs of define-syntaxes, mostly)
    (EE/LetLifts
     [((? EE)) $1]
     [(EE lift/let-loop (? EE/LetLifts))
      (let ([initial (wderiv-e1 $1)]
            [final (wderiv-e2 $3)])
        (make lift/let-deriv initial final $1 $2 $3))])

    ;; Evaluation
    ;; Answer = (listof LocalAction)
    (Eval
     (#:skipped null)
     [((? LocalActions)) $1])

    ;; Prepare env for compilation
    (PrepareEnv
     [(prepare-env (? Eval)) $2])

    ;; Expansion of an expression to primitive form
    (CheckImmediateMacro
     [(enter-check (? CheckImmediateMacro/Inner) exit-check)
      ($2 $1 $3)])
    (CheckImmediateMacro/Inner
    (#:args le1 e2)
     [(!)
      (make p:stop le1 e2 null $1)]
     [(visit Resolves (? MacroStep) return (? CheckImmediateMacro/Inner))
      ($3 $1 $2 ($5 $4 e2))]
     [(visit Resolves tag (? MacroStep) return (? CheckImmediateMacro/Inner))
      (let ([mnode ($4 $3 $2 ($6 $5 e2))])
        (make tagrule $1 (wderiv-e2 mnode) $3 mnode))])

    ;; Expansion of multiple expressions, next-separated
    (NextEEs
     (#:skipped null)
     [() null]
     [(next (? EE) (? NextEEs)) (cons $2 $3)])

    ;; EE

    ;; Expand expression (term)
    (EE
     [(visit Resolves (? EE/k))
      ($3 $1 $2)]
     [(visit Resolves tag (? EE/k))
      (let ([next ($4 $3 $2)])
        (make tagrule $1 (wderiv-e2 next) $3 next))])

    (EE/k
     (#:args e1 rs)
     [(!!)
      (make p:unknown e1 #f rs $1)]
     [(variable return)
      (make p:variable e1 $2 rs #f)]
     [(enter-prim (? Prim) exit-prim return)
      (begin
        (unless (eq? $3 $4)
          (eprintf "warning: exit-prim and return differ:\n~s\n~s\n" $3 $4))
        ($2 $1 $3 rs))]
     [((? MacroStep) (? EE))
      ($1 e1 rs $2)])

    (MacroStep
     (#:args e1 rs next)
     [(enter-macro ! macro-pre-transform (? LocalActions)
                   macro-post-transform ! exit-macro)
      (let ([e2 (and next (wderiv-e2 next))])
        (make mrule e1 e2 rs $2
              $3 $4 (and $5 (car $5)) $6 $7 next))])

    ;; Keyword resolution
    (Resolves
     [() null]
     [(resolve Resolves) (cons $1 $2)])

    ;; Local actions taken by macro
    ;; LocalAction Answer = (list-of LocalAction)
    (LocalActions
     (#:skipped null)
     [() null]
     [((? LocalAction) (? LocalActions)) (cons $1 $2)])

    (LocalAction
     [(!!) (make local-exn $1)]
     [(enter-local OptPhaseUp
       local-pre (? LocalExpand/Inner) OptLifted local-post
       OptOpaqueExpr exit-local)
      (make local-expansion $1 $8 $2 $3 $4 $5 $6 $7)]
     [(lift)
      (make local-lift (cdr $1) (car $1))]
     [(lift-statement)
      (make local-lift-end $1)]
     [(lift-require)
      (make local-lift-require (car $1) (cadr $1) (cddr $1))]
     [(lift-provide)
      (make local-lift-provide $1)]
     [(local-bind ! rename-list exit-local-bind)
      (make local-bind $1 $2 $3 #f)]
     [(local-bind rename-list (? BindSyntaxes) exit-local-bind)
      (make local-bind $1 #f $2 $3)]
     [(track-origin)
      (make track-origin (car $1) (cdr $1))]
     [(local-value ! Resolves local-value-result local-value-binding)
      (make local-value $1 $2 $3 $4 $5)]
     [(local-remark)
      (make local-remark $1)]
     [(local-artificial-step)
      (let ([ids (list-ref $1 0)]
            [before (list-ref $1 1)]
            [mbefore (list-ref $1 2)]
            [mafter (list-ref $1 3)]
            [after (list-ref $1 4)])
        (make local-expansion
          before after #f mbefore
          (make mrule mbefore mafter ids #f
                before null after #f mafter
                (make p:stop mafter mafter null #f))
          #f after #f))]
     [(local-mess)
      ;; Represents subsequence of event stream incoherent due to
      ;; jump (eg, macro catches exn raised from within local-expand).
      (make local-mess $1)]
     ;; -- Not really local actions, but can occur during evaluation
     ;; called 'expand' (not 'local-expand') within transformer
     [(start (? EE)) #f]
     [(start (? CheckImmediateMacro)) #f])

    (LocalExpand/Inner
     [(start (? EE)) $2]
     [((? CheckImmediateMacro)) $1])

    (OptLifted
     [(lift-loop) $1]
     [() #f])
    (OptOpaqueExpr
     [(opaque) $1]
     [() #f])
    (OptPhaseUp
     [(phase-up) #t]
     [() #f])

    (Prim
     (#:args e1 e2 rs)
     [((? PrimModule)) ($1 e1 e2 rs)]
     [((? Prim#%ModuleBegin)) ($1 e1 e2 rs)]
     [((? PrimDefineSyntaxes)) ($1 e1 e2 rs)]
     [((? PrimDefineValues)) ($1 e1 e2 rs)]
     [((? PrimExpression)) ($1 e1 e2 rs)]
     [((? Prim#%App)) ($1 e1 e2 rs)]
     [((? Prim#%Datum)) ($1 e1 e2 rs)]
     [((? Prim#%Top)) ($1 e1 e2 rs)]
     [((? PrimIf)) ($1 e1 e2 rs)]
     [((? PrimWCM)) ($1 e1 e2 rs)]
     [((? PrimSet)) ($1 e1 e2 rs)]
     [((? PrimBegin)) ($1 e1 e2 rs)]
     [((? PrimBegin0)) ($1 e1 e2 rs)]
     [((? PrimLambda)) ($1 e1 e2 rs)]
     [((? PrimCaseLambda)) ($1 e1 e2 rs)]
     [((? PrimLetValues)) ($1 e1 e2 rs)]
     [((? PrimLet*Values)) ($1 e1 e2 rs)]
     [((? PrimLetrecValues)) ($1 e1 e2 rs)]
     [((? PrimLetrecSyntaxes+Values)) ($1 e1 e2 rs)]
     [((? PrimSTOP)) ($1 e1 e2 rs)]
     [((? PrimQuote)) ($1 e1 e2 rs)]
     [((? PrimQuoteSyntax)) ($1 e1 e2 rs)]
     [((? PrimRequire)) ($1 e1 e2 rs)]
     [((? PrimProvide)) ($1 e1 e2 rs)]
     [((? PrimVarRef)) ($1 e1 e2 rs)]
     [((? PrimStratifiedBody)) ($1 e1 e2 rs)]
     [((? PrimBeginForSyntax)) ($1 e1 e2 rs)])

    (PrimModule
     (#:args e1 e2 rs)
     [(prim-module ! (? PrepareEnv) OptTag rename-one
                   (? OptCheckImmediateMacro) OptTag !
                   (? EE) rename-one)
      (make p:module e1 e2 rs $2 $3 $4 $5 $6 $7 $8 $9 $10)])
    (OptTag
     [() #f]
     [(tag) $1])
    (OptCheckImmediateMacro
     [() #f]
     [((? CheckImmediateMacro)) $1])

    ;; FIXME: workaround for problem in expander instrumentation:
    ;;   observer not propagated correctly to expand_all_provides
    ;;   so local actions that should be within prim-provide's EE 
    ;;   instead appear directly here
    (Prim#%ModuleBegin
     (#:args e1 e2 rs)
     [(prim-#%module-begin ! rename-one (? ModuleBegin/Phase) (? Eval) next (? ExpandSubmodules))
      (make p:#%module-begin e1 e2 rs $2 $3 $4
            (for/or ([la (in-list $5)])
              (and (local-exn? la) (local-exn-exn la)))
            $7)])
    #|
    ;; restore this version when expander fixed
    (Prim#%ModuleBegin-REAL
     (#:args e1 e2 rs)
     [(prim-#%module-begin ! rename-one (? ModuleBegin/Phase) ! (? ExpandSubmodules))
      (make p:#%module-begin e1 e2 rs $2 $3 $4 $5)])
    |#
    (ExpandSubmodules
     (#:skipped null)
     [(enter-prim (? PrimModule) exit-prim (? ExpandSubmodules))
      (cons ($2 $1 $3 null) $4)]
     [() null])

    (ModuleBegin/Phase
     [((? ModulePass1) next-group (? ModulePass2) next-group (? ModulePass3))
      (make module-begin/phase $1 $3 $5)])

    (ModulePass1
     (#:skipped null)
     [() null]
     [(next (? ModulePass1-Part) (? ModulePass1))
      (cons $2 $3)]
     [(module-lift-end-loop (? ModulePass1))
      (cons (make mod:lift-end $1) $2)])

    (ModulePass1-Part
     [((? EE) rename-one (? ModulePass1/Prim))
      (make mod:prim $1 $2 ($3 $2))]
     [(EE rename-one ! splice)
      (make mod:splice $1 $2 $3 $4)]
     [(EE rename-list module-lift-loop)
      (make mod:lift $1 null $2 $3)])

    (ModulePass1/Prim
     (#:args e1)
     [(enter-prim prim-define-values ! exit-prim)
      (make p:define-values $1 $4 null $3 #f)]
     [(enter-prim prim-define-syntaxes ! (? PrepareEnv)
                  phase-up (? EE/LetLifts) (? Eval) exit-prim)
      (make p:define-syntaxes $1 $8 null $3 $4 $6 $7)]
     [(enter-prim prim-begin-for-syntax ! (? PrepareEnv)
                  phase-up (? ModuleBegin/Phase) (? Eval) exit-prim)
      (make p:begin-for-syntax $1 $7 null $3 $4 $6 $7)]
     [(enter-prim prim-require (? Eval) exit-prim)
      (make p:require $1 $4 null #f $3)]
     [(enter-prim prim-submodule ! (? ExpandSubmodules #|one|#) exit-prim)
      (make p:submodule $1 $5 null $3 (car $4))]
     [(enter-prim prim-submodule* ! exit-prim)
      (make p:submodule* $1 $4 null $3)]
     [()
      (make p:stop e1 e1 null #f)])

    (ModulePass2
     (#:skipped null)
     [() null]
     [(next (? ModulePass2-Part) (? ModulePass2))
      (cons $2 $3)]
     [(module-lift-end-loop (? ModulePass2))
      (cons (make mod:lift-end $1) $2)])

    (ModulePass2-Part
     ;; not normal; already handled
     [()
      (make mod:skip)]
     ;; normal: expand completely
     [((? EE) (? Eval))
      ;; after expansion, may compile => may eval letstx rhss again!
      ;; need to include those evals too (for errors, etc)
      (make mod:cons $1 $2)]
     ;; catch lifts
     [(EE Eval module-lift-loop)
      ;; same as above: after expansion, may compile => may eval
      (make mod:lift $1 $2 #f $3)])

    (ModulePass3
     (#:skipped null)
     [() null]
     [((? ModulePass3-Part) (? ModulePass3))
      (cons $1 $2)])

    (ModulePass3-Part
     [(enter-prim prim-provide (? ModuleProvide/Inner) ! exit-prim)
      (make p:provide $1 $5 null #f $3 $4)])

    (ModuleProvide/Inner
     (#:skipped null)
     [() null]
     [((? EE) (? ModuleProvide/Inner))
      (cons $1 $2)])

    ;; Definitions
    (PrimDefineSyntaxes
     (#:args e1 e2 rs)
     [(prim-define-syntaxes ! (? PrepareEnv) (? EE/LetLifts) (? Eval))
      (make p:define-syntaxes e1 e2 rs $2 $3 $4 $5)])

    (PrimDefineValues
     (#:args e1 e2 rs)
     [(prim-define-values ! (? EE))
      (make p:define-values e1 e2 rs $2 $3)])

    ;; Simple expressions
    (PrimExpression
     (#:args e1 e2 rs)
     [(prim-expression ! (? EE))
      (make p:#%expression e1 e2 rs $2 $3 #f)]
     [(prim-expression EE tag)
      (make p:#%expression e1 e2 rs #f $2 $3)])

    (PrimIf
     (#:args e1 e2 rs)
     [(prim-if ! (? EE) next (? EE) next (? EE))
      (make p:if e1 e2 rs $2 $3 $5 $7)])

    (PrimWCM 
     (#:args e1 e2 rs)
     [(prim-wcm ! (? EE) next (? EE) next (? EE))
      (make p:wcm e1 e2 rs $2 $3 $5 $7)])

    ;; Sequence-containing expressions
    (PrimBegin
     (#:args e1 e2 rs)
     [(prim-begin ! (? EL))
      (make p:begin e1 e2 rs $2 $3)])

    (PrimBegin0
     (#:args e1 e2 rs)
     [(prim-begin0 ! next (? EE) next (? EL))
      (make p:begin0 e1 e2 rs $2 $4 $6)])

    (Prim#%App
     (#:args e1 e2 rs)
     [(prim-#%app !)
      (make p:#%app e1 e2 rs $2 #f)]
     [(prim-#%app (? EL))
      (make p:#%app e1 e2 rs #f $2)])

    ;; Binding expressions
    (PrimLambda
     (#:args e1 e2 rs)
     [(prim-lambda ! renames-lambda (? EB))
      (make p:lambda e1 e2 rs $2 $3 $4)])

    (PrimCaseLambda
     (#:args e1 e2 rs)
     [(prim-case-lambda ! (? NextCaseLambdaClauses))
      (make p:case-lambda e1 e2 rs $2 $3)])

    (NextCaseLambdaClauses
     (#:skipped null)
     [(next (? CaseLambdaClause) (? NextCaseLambdaClauses))
      (cons $2 $3)]
     [() null])

    (CaseLambdaClause
     [(! renames-case-lambda (? EB))
      (make clc $1 $2 $3)])

    (PrimLetValues
     (#:args e1 e2 rs)
     [(prim-let-values ! renames-let (? NextEEs) next-group (? EB))
      (make p:let-values e1 e2 rs $2 $3 $4 $6)])

    (PrimLet*Values
     (#:args e1 e2 rs)
     ;; let*-values with bindings is "macro-like"
     [(prim-let*-values !!)
      (make mrule e1 e2 rs $2 #f null #f #f #f #f)]
     [(prim-let*-values (? EE))
      (let* ([next-e1 (wderiv-e1 $2)])
        (make mrule e1 e2 rs #f e1 null next-e1 #f next-e1 $2))]
     ;; No bindings... model as "let"
     [(prim-let*-values renames-let (? NextEEs) next-group (? EB))
      (make p:let-values e1 e2 rs #f $2 $3 $5)])

    (PrimLetrecValues
     (#:args e1 e2 rs)
     [(prim-letrec-values ! renames-let (? NextEEs) next-group (? EB))
      (make p:letrec-values e1 e2 rs $2 $3 $4 $6)])

    (PrimLetrecSyntaxes+Values
     (#:args e1 e2 rs)
     [(prim-letrec-syntaxes+values ! renames-letrec-syntaxes
       (? PrepareEnv) (? NextBindSyntaxess) next-group (? EB) OptTag)
      (make p:letrec-syntaxes+values e1 e2 rs $2 $3 $4 $5 #f null $7 $8)]
     [(prim-letrec-syntaxes+values renames-letrec-syntaxes 
       PrepareEnv NextBindSyntaxess next-group
       prim-letrec-values
       renames-let (? NextEEs) next-group (? EB) OptTag)
      (make p:letrec-syntaxes+values e1 e2 rs #f $2 $3 $4 $7 $8 $10 $11)])

    ;; Atomic expressions
    (Prim#%Datum
     (#:args e1 e2 rs)
     [(prim-#%datum !) (make p:#%datum e1 e2 rs $2)])

    (Prim#%Top
     (#:args e1 e2 rs)
     [(prim-#%top !) (make p:#%top e1 e2 rs $2)])

    (PrimSTOP
     (#:args e1 e2 rs)
     [(prim-stop !) (make p:stop e1 e2 rs $2)])

    (PrimQuote
     (#:args e1 e2 rs)
     [(prim-quote !) (make p:quote e1 e2 rs $2)])

    (PrimQuoteSyntax
     (#:args e1 e2 rs)
     [(prim-quote-syntax !) (make p:quote-syntax e1 e2 rs $2)])

    (PrimRequire
     (#:args e1 e2 rs)
     [(prim-require (? Eval))
      (make p:require e1 e2 rs #f $2)])

    (PrimProvide 
     (#:args e1 e2 rs)
     [(prim-provide !) (make p:provide e1 e2 rs $2 null #f)])

    (PrimVarRef
     (#:args e1 e2 rs)
     [(prim-varref !) (make p:#%variable-reference e1 e2 rs $2)])

    (PrimStratifiedBody
     (#:args e1 e2 rs)
     [(prim-#%stratified-body ! (? EB)) (make p:#%stratified-body e1 e2 rs $2 $3)])

    (PrimBeginForSyntax
     (#:args e1 e2 rs)
     [(prim-begin-for-syntax ! (? PrepareEnv) (? BeginForSyntax*) (? Eval))
      (make p:begin-for-syntax e1 e2 rs $2 $3 $4 $5)])
    (BeginForSyntax*
     [((? EL))
      (list $1)]
     [(EL module-lift-loop (? BeginForSyntax*))
      (cons (make bfs:lift $1 $2) $3)])

    (PrimSet
     (#:args e1 e2 rs)
     ;; Unrolled to avoid shift/reduce
     [(prim-set! ! resolve Resolves ! next (? EE))
      (make p:set! e1 e2 rs $2 (cons $3 $4) $5 $7)]
     [(prim-set! Resolves (? MacroStep) (? EE))
      (make p:set!-macro e1 e2 rs #f ($3 e1 $2 $4))])

    ;; Blocks
    ;; EB Answer = BlockDerivation
    (EB
     [(enter-block (? BlockPass1) block->list (? EL))
      (make bderiv $1 (and $4 (wlderiv-es2 $4))
            $2 'list $4)]
     [(enter-block BlockPass1 block->letrec (? EE))
      (make bderiv $1 (and $4 (list (wderiv-e2 $4)))
            $2 'letrec $4)])

    ;; BlockPass1 Answer = (list-of BRule)
    (BlockPass1
     (#:skipped null)
     [() null]
     [((? BRule) (? BlockPass1))
      (cons $1 $2)])

    ;; BRule Answer = BRule
    (BRule
     [(next !!)
      (make b:error $2)]
     [(next renames-block (? CheckImmediateMacro))
      (make b:expr $2 $3)]
     [(next renames-block CheckImmediateMacro prim-begin ! splice !)
      (make b:splice $2 $3 $5 $6 $7)]
     [(next renames-block CheckImmediateMacro prim-define-values ! rename-one !)
      (make b:defvals $2 $3 $5 $6 $7)]
     [(next renames-block CheckImmediateMacro
            prim-define-syntaxes ! rename-one ! (? PrepareEnv) (? BindSyntaxes))
      (make b:defstx $2 $3 $5 $6 $7 $8 $9)])

    ;; BindSyntaxes Answer = Derivation
    (BindSyntaxes
     [(enter-bind (? EE/LetLifts) next (? Eval) exit-bind)
      (make bind-syntaxes $2 $4)])

    ;; NextBindSyntaxess Answer = (list-of Derivation)
    (NextBindSyntaxess
     (#:skipped null)
     [() null]
     [(next (? BindSyntaxes) (? NextBindSyntaxess)) (cons $2 $3)])

    ;; Lists
    ;; EL Answer = ListDerivation
    (EL
     (#:skipped #f)
     [(enter-list ! (? EL*) exit-list)
      ;; FIXME: Workaround for bug in events
      (if (null? $3)
          (make lderiv null null $2 $3)
          (make lderiv $1 $4 $2 $3))])

    ;; EL* Answer = (listof Derivation)
    (EL*
     (#:skipped null)
     [() null]
     [(next (? EE) (? EL*)) (cons $2 $3)])

    )))
