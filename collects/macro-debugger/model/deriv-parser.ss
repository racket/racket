
(module deriv-parser mzscheme
  (require "yacc-ext.ss"
           "yacc-interrupted.ss"
           "deriv.ss"
           "deriv-util.ss"
           "deriv-tokens.ss")
  (provide parse-derivation)

  (define (deriv-error ok? name value start end)
    (if ok?
        (error 'derivation-parser
               "error on token #~a: <~s, ~s>"
               start name value)
        (error 'derivation-parser "bad token #~a" start)))

  ;; PARSER

  (define (parse-derivation x)
    (parameterize ((current-sequence-number 0))
      (parse-derivation* x)))

  (define current-sequence-number (make-parameter #f))
  (define (new-sequence-number)
    (let ([seq (current-sequence-number)])
      (current-sequence-number (add1 seq))
      seq))

  (define-struct (exn:eval exn) (deriv))
  (define empty-cms
    (call-with-continuation-prompt (lambda () (current-continuation-marks))))
  (define (create-eval-exn deriv)
    (make-exn:eval "exception during evaluation"
                   empty-cms
                   deriv))
  
  (define-production-splitter production/I values values)

  (define-syntax (productions/I stx)
    (syntax-case stx ()
      [(productions/I def ...)
       #'(begin (production/I def) ...)]))

  (define parse-derivation*
    (parser
     (options (start Expansion)
              (src-pos)
              (tokens basic-tokens prim-tokens renames-tokens)
              (end EOF)
              (error deriv-error)
              #;(debug "DEBUG-PARSER.txt"))

     ;; tokens
     (skipped-token-values
      visit resolve next next-group return
      enter-macro macro-pre-transform macro-post-transform exit-macro 
      enter-prim exit-prim
      enter-block block->list block->letrec splice
      enter-list exit-list
      enter-check exit-check
      local-post exit-local exit-local/expr
      phase-up module-body
      renames-lambda
      renames-case-lambda
      renames-let
      renames-letrec-syntaxes
      renames-block
      IMPOSSIBLE)

      ;; Entry point
     (productions
      (Expansion
       [(start EE/Lifts) $2]
       [(start EE/Lifts/Interrupted) $2]))

     (productions/I

      ;; Expand/Lifts
      (EE/Lifts
       (#:no-wrap)
       [((? EE)) $1]
       [((? EE/Lifts+)) $1])

      (EE/Lifts+
       (#:no-wrap)
       [(EE lift-loop (? EE/Lifts))
        (let ([e1 (wderiv-e1 $1)]
              [e2 (wderiv-e2 $3)])
          (make lift-deriv e1 e2 $1 $2 $3))])

      ;; Expansion of an expression
      ;; EE Answer = Derivation (I)
      (EE
       (#:no-wrap)
       [(visit (? PrimStep) return)
        ($2 $1 $3)]
       [((? EE/Macro))
        $1])

      (EE/Macro
       (#:wrap)
       [(visit (? MacroStep) (? EE))
        (make mrule $1 (and $3 (wderiv-e2 $3)) $2 $3)])

      ;; Expand/LetLifts
      ;; Used for expand_lift_to_let (rhs of define-syntaxes, mostly)
      (EE/LetLifts
       (#:no-wrap)
       [((? EE)) $1]
       [((? EE/LetLifts+)) $1])

      (EE/LetLifts+
       (#:wrap)
       [(EE lift/let-loop (? EE/LetLifts))
        (let ([initial (wderiv-e1 $1)]
              [final (wderiv-e2 $3)])
          (make lift/let-deriv initial final $1 $2 $3))])

      ;; Evaluation
      ;; Answer = ?exn
      (Eval
       (#:no-wrap)
       [() #f]
       [(!!) $1]
       [(start EE/Interrupted) (create-eval-exn $2)]
       [(start EE (? Eval)) $3]
       [(start CheckImmediateMacro/Interrupted) (create-eval-exn $2)]
       [(start CheckImmediateMacro (? Eval)) $3])
      
      ;; Expansion of an expression to primitive form
      (CheckImmediateMacro
       (#:no-wrap)
       [(enter-check (? CheckImmediateMacro/Inner) exit-check)
        ($2 $1 $3 (lambda (ce1 ce2) (make p:stop ce1 ce2 null #f)))])
      (CheckImmediateMacro/Inner
       (#:args e1 e2 k)
       (#:wrap)
       [()
        (k e1 e2)]
       [(visit (? MacroStep) return (? CheckImmediateMacro/Inner))
        (let ([next ($4 $3 e2 k)])
          (make mrule $1 (and next (wderiv-e2 next)) $2 next))])

      ;; Expansion of multiple expressions, next-separated
      (NextEEs
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? EE) (? NextEEs)) (cons $2 $3)])

      ;; Keyword resolution
      (Resolves
       (#:no-wrap)
       [() null]
       [(resolve Resolves) (cons $1 $2)])

      ;; Single macro step (may contain local-expand calls)
      ;; MacroStep Answer = Transformation (I,E)
      (MacroStep
       (#:wrap)
       [(Resolves enter-macro ! macro-pre-transform (? LocalActions)
                  ! macro-post-transform exit-macro)
        (make transformation $2 $8 $1 $3 $4 $5 $6 $7 (new-sequence-number))])

      ;; Local actions taken by macro
      ;; LocalAction Answer = (list-of LocalAction)
      (LocalActions
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [((? LocalAction) (? LocalActions)) (cons $1 $2)]
       [((? NotReallyLocalAction) (? LocalActions)) $2])

      (LocalAction
       (#:no-wrap)
       [(enter-local local-pre start (? EE) local-post exit-local)
        (make local-expansion $1 $6 $2 $5 #f $4)]
       [(enter-local phase-up local-pre start (? EE) local-post exit-local)
        (make local-expansion $1 $7 $3 $6 #t $5)]
       [(enter-local/expr local-pre start (? EE) local-post exit-local/expr)
        (make local-expansion/expr $1 (car $6) $2 $5 #f (cdr $6) $4)]
       [(enter-local/expr local-pre phase-up start (? EE) local-post exit-local/expr)
        (make local-expansion/expr $1 (car $7) $3 $6 #t (cdr $7) $5)]
       [(lift)
        (make local-lift (cdr $1) (car $1))]
       [(lift-statement)
        (make local-lift-end $1)]
       [((? BindSyntaxes))
        (make local-bind $1)])

      (NotReallyLocalAction
       (#:no-wrap)
       ;; called 'expand' (not 'local-expand') within transformer
       [(start (? EE))
        (make local-expansion (wderiv-e1 $2)
                              (wderiv-e2 $2)
                              (wderiv-e1 $2)
                              (wderiv-e2 $2)
                              #f
                              $2)])

      ;; Primitive
      (PrimStep
       (#:args e1 e2)
       (#:no-wrap)
       [(Resolves (? PrimError))
        ($2 e1 e2 $1)]
       [(Resolves Variable)
        ($2 e1 e2 $1)]
       [(Resolves enter-prim (? Prim) exit-prim)
        ($3 e1 e2 $1)]
       [(Resolves enter-prim (? TaggedPrim) exit-prim)
        ($3 e1 $4 $1 $2)])

      (PrimError
       (#:args e1 e2 rs)
       (#:wrap)
       [(! IMPOSSIBLE)
        (make p:unknown e1 e2 rs $1)])

      (Variable
       (#:args e1 e2 rs)
       (#:wrap)
       [(variable)
        (make p:variable e1 e2 rs #f)])

      (TaggedPrim
       (#:args e1 e2 rs tagged-stx)
       (#:no-wrap)
       [((? Prim#%App)) ($1 e1 e2 rs tagged-stx)]
       [((? Prim#%Datum)) ($1 e1 e2 rs tagged-stx)]
       [((? Prim#%Top)) ($1 e1 e2 rs tagged-stx)])

      (Prim
       (#:args e1 e2 rs)
       (#:no-wrap)
       [((? PrimModule)) ($1 e1 e2 rs)]
       [((? Prim#%ModuleBegin)) ($1 e1 e2 rs)]
       [((? PrimDefineSyntaxes)) ($1 e1 e2 rs)]
       [((? PrimDefineValues)) ($1 e1 e2 rs)]
       [((? PrimExpression)) ($1 e1 e2 rs)]
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
       [((? PrimRequireForSyntax)) ($1 e1 e2 rs)]
       [((? PrimRequireForTemplate)) ($1 e1 e2 rs)]
       [((? PrimProvide)) ($1 e1 e2 rs)])

      (PrimModule
       (#:args e1 e2 rs)
       (#:wrap)
       ;; Multiple forms after language: tagging done automatically
       [(prim-module (? Eval) (? EE))
        (make p:module e1 e2 rs $2 #f #f #f $3)]
       ;; One form after language: macro that expands into #%module-begin
       [(prim-module Eval next (? CheckImmediateMacro) next ! (? EE))
        (make p:module e1 e2 rs #f #t $4 $6 $7)])
      
      (Prim#%ModuleBegin
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-#%module-begin ! (? ModulePass1) next-group (? ModulePass2) !)
        (make p:#%module-begin e1 e2 rs $2 $3 $5 $6)])

      (ModulePass1
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? ModulePass1-Part) (? ModulePass1))
        (cons $2 $3)]
       [(module-lift-end-loop (? ModulePass1))
        (cons (make mod:lift-end $1) $2)])

      (ModulePass1-Part
       (#:wrap)
       [((? EE) (? ModulePass1/Prim))
        (make mod:prim $1 $2)]
       [(EE ! splice)
        (make mod:splice $1 $2 $3)]
       [(EE module-lift-loop)
        (make mod:lift $1 $2)])

      (ModulePass1/Prim
       (#:wrap)
       [(enter-prim prim-define-values ! exit-prim)
        (make p:define-values $1 $4 null $3 #f)]
       [(enter-prim prim-define-syntaxes !
                    phase-up (? EE/LetLifts) (? Eval) exit-prim)
        (make p:define-syntaxes $1 $7 null $3 $5 $6)]
       [(enter-prim prim-require (? Eval) exit-prim)
        (make p:require $1 $4 null $3)]
       [(enter-prim prim-require-for-syntax (? Eval) exit-prim)
        (make p:require-for-syntax $1 $4 null $3)]
       [(enter-prim prim-require-for-template (? Eval) exit-prim)
        (make p:require-for-template $1 $4 null $3)]
       [(enter-prim prim-provide ! exit-prim)
        (make p:provide $1 $4 null $3)]
       [()
        #f])

      (ModulePass2
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? ModulePass2-Part) (? ModulePass2))
        (cons $2 $3)]
       [(module-lift-end-loop (? ModulePass2))
        (cons (make mod:lift-end $1) $2)])

      (ModulePass2-Part
       (#:no-wrap)
       ;; not normal; already handled
       [()
        (make mod:skip)]
       ;; normal: expand completely
       [((? EE))
        (make mod:cons $1)]
       ;; catch lifts
       [(EE module-lift-loop)
        (make mod:lift $1 $2)])

      ;; Definitions
      (PrimDefineSyntaxes
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-define-syntaxes ! (? EE/LetLifts) (? Eval))
        (make p:define-syntaxes e1 e2 rs $2 $3 $4)])

      (PrimDefineValues
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-define-values ! (? EE))
        (make p:define-values e1 e2 rs $2 $3)])

      ;; Simple expressions
      (PrimExpression
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-expression ! (? EE))
        (make p:#%expression e1 e2 rs $2 $3)])

      (PrimIf
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-if ! (? EE) next (? EE) next (? EE))
        (make p:if e1 e2 rs $2 #t $3 $5 $7)]
       [(prim-if next-group (? EE) next (? EE))
        (make p:if e1 e2 rs #f #f $3 $5 #f)])

      (PrimWCM 
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-wcm ! (? EE) next (? EE) next (? EE))
        (make p:wcm e1 e2 rs $2 $3 $5 $7)])

      ;; Sequence-containing expressions
      (PrimBegin
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-begin ! (? EL))
        (make p:begin e1 e2 rs $2 $3)])

      (PrimBegin0
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-begin0 ! next (? EE) next (? EL))
        (make p:begin0 e1 e2 rs $2 $4 $6)])

      (Prim#%App
       (#:args e1 e2 rs tagged-stx)
       (#:wrap)
       [(prim-#%app !)
        (make p:#%app e1 e2 rs $2 tagged-stx (make lderiv null null #f null))]
       [(prim-#%app (? EL))
        (make p:#%app e1 e2 rs #f tagged-stx $2)])

      ;; Binding expressions
      (PrimLambda
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-lambda ! renames-lambda (? EB))
        (make p:lambda e1 e2 rs $2 $3 $4)])

      (PrimCaseLambda
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-case-lambda ! (? NextCaseLambdaClauses))
        (make p:case-lambda e1 e2 rs $2 $3)])

      (NextCaseLambdaClauses
       (#:skipped null)
       (#:no-wrap)
       [(next (? CaseLambdaClause) (? NextCaseLambdaClauses))
        (cons $2 $3)]
       [() null])

      (CaseLambdaClause
       (#:wrap)
       [(! renames-case-lambda (? EB))
        (make clc $1 $2 $3)])

      (PrimLetValues
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-let-values ! renames-let (? NextEEs) next-group (? EB))
        (make p:let-values e1 e2 rs $2 $3 $4 $6)])

      (PrimLet*Values
       (#:args e1 e2 rs)
       (#:wrap)
       ;; let*-values with bindings is "macro-like"
       [(prim-let*-values !!)
        (let ([tx (make transformation e1 #f rs $2
                        #f null #f #f (new-sequence-number))])
          (make mrule e1 e2 tx #f))]
       [(prim-let*-values (? EE))
        (let* ([next-e1 (wderiv-e1 $2)]
               [tx (make transformation e1 next-e1 rs #f
                         e1 null #f next-e1 (new-sequence-number))])
          (make mrule e1 e2 tx $2))]
       ;; No bindings... model as "let"
       [(prim-let*-values renames-let (? NextEEs) next-group (? EB))
        (make p:let-values e1 e2 rs #f $2 $3 $5)])

      (PrimLetrecValues
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-letrec-values ! renames-let (? NextEEs) next-group (? EB))
        (make p:letrec-values e1 e2 rs $2 $3 $4 $6)])

      (PrimLetrecSyntaxes+Values
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-letrec-syntaxes+values ! renames-letrec-syntaxes
        (? NextBindSyntaxess) next-group (? EB))
        (make p:letrec-syntaxes+values e1 e2 rs $2 $3 $4 #f null $6)]
       [(prim-letrec-syntaxes+values renames-letrec-syntaxes
         NextBindSyntaxess next-group
         prim-letrec-values
         renames-let (? NextEEs) next-group (? EB))
        (make p:letrec-syntaxes+values e1 e2 rs #f $2 $3 $6 $7 $9)])

      ;; Atomic expressions
      (Prim#%Datum
       (#:args e1 e2 rs tagged-stx)
       (#:wrap)
       [(prim-#%datum !) (make p:#%datum e1 e2 rs $2 tagged-stx)])

      (Prim#%Top
       (#:args e1 e2 rs tagged-stx)
       (#:wrap)
       [(prim-#%top !) (make p:#%top e1 e2 rs $2 tagged-stx)])

      (PrimSTOP
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-stop !) (make p:stop e1 e2 rs $2)])

      (PrimQuote
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-quote !) (make p:quote e1 e2 rs $2)])

      (PrimQuoteSyntax
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-quote-syntax !) (make p:quote-syntax e1 e2 rs $2)])

      (PrimRequire
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-require (? Eval))
        (make p:require e1 e2 rs $2)])

      (PrimRequireForSyntax
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-require-for-syntax (? Eval))
        (make p:require-for-syntax e1 e2 rs $2)])

      (PrimRequireForTemplate
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-require-for-template (? Eval))
        (make p:require-for-template e1 e2 rs $2)])

      (PrimProvide 
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-provide !) (make p:provide e1 e2 rs $2)])

      (PrimSet
       (#:args e1 e2 rs)
       (#:wrap)
       [(prim-set! ! Resolves next (? EE))
        (make p:set! e1 e2 rs $2 $3 $5)]
       [(prim-set! (? MacroStep) (? EE))
        (make p:set!-macro e1 e2 rs #f
              (make mrule e1 (and $3 (wderiv-e2 $3)) $2 $3))])

      ;; Blocks
      ;; EB Answer = BlockDerivation
      (EB
       (#:wrap)
       [(enter-block (? BlockPass1) block->list (? EL))
        (make bderiv $1 (and $4 (wlderiv-es2 $4))
              $2 'list $4)]
       [(enter-block BlockPass1 block->letrec (? EL))
        (make bderiv $1 (and $4 (wlderiv-es2 $4))
              $2 'letrec $4)])

      ;; BlockPass1 Answer = (list-of BRule)
      (BlockPass1
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [((? BRule) (? BlockPass1))
        (cons $1 $2)])

      ;; BRule Answer = BRule
      (BRule
       (#:wrap)
       [(next !!)
        (make b:error $2)]
       [(next renames-block (? CheckImmediateMacro))
        (make b:expr $2 $3)]
       [(next renames-block CheckImmediateMacro prim-begin ! splice !)
        (make b:splice $2 $3 $5 $6 $7)]
       [(next renames-block CheckImmediateMacro prim-define-values !)
        (make b:defvals $2 $3 $5)]
       [(next renames-block CheckImmediateMacro
              prim-define-syntaxes ! (? BindSyntaxes))
        (make b:defstx $2 $3 $5 $6)])

      ;; BindSyntaxes Answer = Derivation
      (BindSyntaxes
       (#:wrap)
       [(phase-up (? EE/LetLifts) (? Eval))
        (make bind-syntaxes $2 $3)])

      ;; NextBindSyntaxess Answer = (list-of Derivation)
      (NextBindSyntaxess
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? BindSyntaxes) (? NextBindSyntaxess)) (cons $2 $3)])

      ;; Lists
      ;; EL Answer = ListDerivation
      (EL
       (#:wrap)
       (#:skipped #f)
       [(enter-list ! (? EL*) exit-list)
        ;; FIXME: Workaround for bug in events
        (if (null? $3)
            (make lderiv null null $2 $3)
            (make lderiv $1 $4 $2 $3))])

      ;; EL* Answer = (listof Derivation)
      (EL*
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? EE) (? EL*)) (cons $2 $3)])

      )))

  )
