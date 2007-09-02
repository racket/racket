
(module deriv-parser mzscheme
  (require "yacc-ext.ss"
           "yacc-interrupted.ss"
           "deriv.ss"
           "deriv-util.ss"
           "deriv-tokens.ss")
  (provide parse-derivation)
  
  (define (deriv-error ok? name value start end)
    (if ok?
        (error 'derivation-parser "error on token #~a: <~s, ~s>" start name value)
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
  
  (define parse-derivation*
    (parser
     (options (start Expansion)
              (src-pos)
              (tokens basic-tokens prim-tokens renames-tokens)
              (end EOF)
              (error deriv-error)
              #;(debug "debug-parser.txt"))

     ;; Required (non-hygienically) by productions/I
     (productions
      #;(Error [(syntax-error) $1])
      (NoError [() #f]))
     
     ;; tokens
     (skipped-token-values visit resolve next next-group return
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

      ;; Expansion of an expression
      ;; EE Answer = Derivation (I)
      (EE
       (#:no-wrap)
       [(visit (? PrimStep 'prim) return)
        $2]
       [(visit (? TaggedPrimStep 'prim) return)
        ($2 $1)]
       [(visit VariableStep return)
        ($2 $1 $3)]
       [((? EE/Macro))
        $1])
      (EE/Macro
       [(visit (? MacroStep 'macro) (? EE 'next))
        (make-mrule $1 (and (deriv? $3) (deriv-e2 $3)) $2 $3)])

      ;; Expand/Lifts
      ;; Expand/Lifts Answer = Derivation (I)
      (EE/Lifts
       (#:no-wrap)
       [((? EE)) $1]
       [((? EE/Lifts+)) $1])
      (EE/Lifts+
       [(EE lift-loop (? EE/Lifts))
        (let ([initial (deriv-e1 $1)]
              [final (and (deriv? $3) (deriv-e2 $3))])
          (make-lift-deriv initial final $1 $2 $3))])


      ;; Expand/LetLifts
      ;; Expand/LetLifts Answer = Derivation (I)
      ;; Used for expand_lift_to_let (rhs of define-syntaxes, mostly)
      (EE/LetLifts
       (#:no-wrap)
       [((? EE)) $1]
       [((? EE/LetLifts+)) $1])
      (EE/LetLifts+
       [(EE lift/let-loop (? EE/LetLifts))
        (let ([initial (deriv-e1 $1)]
              [final (and (deriv? $3) (deriv-e2 $3))])
          (make-lift/let-deriv initial final $1 $2 $3))])
      
      ;; Evaluation
      (Eval
       (#:no-wrap)
       [() #f]
       [(start (? EE) (? Eval)) #f]
       [(start (? CheckImmediateMacro) (? Eval)) #f])
      
      ;; Expansion of an expression to primitive form
      ;; CheckImmediateMacro Answer = Derivation (I)
      (CheckImmediateMacro
       (#:no-wrap)
       [(enter-check (? CheckImmediateMacro/Inner) exit-check)
        ($2 $1 $3 (lambda (ce1 ce2) (make-p:stop ce1 ce2 null)))])
      (CheckImmediateMacro/Inner
       (#:args e1 e2 k)
       [()
        (k e1 e2)]
       [(visit (? MacroStep 'macro) return (? CheckImmediateMacro/Inner 'next))
        (let ([next ($4 $3 e2 k)])
          (make-mrule $1 (and (deriv? next) (deriv-e2 next)) $2 next))])

      ;; Expansion of multiple expressions, next-separated
      ;; NextEEs Answer = (listof Derivation)
      (NextEEs
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? EE 'first) (? NextEEs 'rest)) (cons $2 $3)])

      ;; Keyword resolution
      ;; Resolves Answer = (listof identifier)
      (Resolves [() null]
                [(resolve Resolves) (cons $1 $2)])
      
      ;; Single macro step (may contain local-expand calls)
      ;; MacroStep Answer = Transformation (I,E)
      (MacroStep 
       [(Resolves enter-macro 
         (! 'bad-transformer)
         macro-pre-transform (? LocalActions 'locals) (! 'transform) macro-post-transform 
         exit-macro)
        (make-transformation $2 $8 $1 $4 $7 $5 (new-sequence-number))])

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
        (make-local-expansion $1 $6 $2 $5 #f $4)]
       [(enter-local phase-up local-pre start (? EE) local-post exit-local)
        (make-local-expansion $1 $7 $3 $6 #t $5)]
       [(enter-local/expr local-pre start (? EE) local-post exit-local/expr)
        (make-local-expansion/expr $1 (car $6) $2 $5 #f (cdr $6) $4)]
       [(enter-local/expr local-pre phase-up start (? EE) local-post exit-local/expr)
        (make-local-expansion/expr $1 (car $7) $3 $6 #t (cdr $7) $5)]
       [(lift)
        (make-local-lift (cdr $1) (car $1))]
       [(lift-statement)
        (make-local-lift-end $1)]
       [(phase-up (? EE/LetLifts))
        (make-local-bind $2)])

      (NotReallyLocalAction
       (#:no-wrap)
       ;; called 'expand' (not 'local-expand') within transformer
       [(start (? EE))
        (make-local-expansion (lift/deriv-e1 $2)
                              (lift/deriv-e2 $2)
                              (lift/deriv-e1 $2)
                              (lift/deriv-e2 $2)
                              #f
                              $2)])
      
      ;; Multiple calls to local-expand
      ;; EEs Answer = (listof Derivation)
      (EEs
       (#:skipped null)
       (#:no-wrap)
       [() null]
       [((? EE 'first) (? EEs 'rest)) (cons $1 $2)])
      
      ;; Primitive syntax step
      ;; PrimStep Answer = PRule
      (PrimStep
       (#:no-wrap)
       [(Resolves NoError enter-prim (? Prim) exit-prim)
        ($4 $3 $5 $1)])

      (VariableStep
       (#:no-wrap)
       (#:args e1 e2)
       [(Resolves variable)
        (make-p:variable e1 e2 $1)])
      
      ;; Tagged Primitive syntax
      ;; TaggedPrimStep Answer = syntax -> PRule
      (TaggedPrimStep
       (#:no-wrap)
       (#:args orig-stx)
       [(Resolves ! IMPOSSIBLE)
        (make-p:unknown orig-stx #f $1)]
       [(Resolves NoError enter-prim ! IMPOSSIBLE)
        (make-p:unknown orig-stx #f $1)]
       [(Resolves NoError enter-prim (? TaggedPrim) exit-prim)
        ($4 orig-stx $5 $1 $3)])

      ;; Primitive
      ;; Prim Answer = syntax syntax (listof identifier) -> PRule
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
      
      ;; Tagged Primitive
      ;; TaggedPrim Answer = syntax syntax (list-of identifier) syntax -> PRule
      (TaggedPrim
       (#:args e1 e2 rs tagged-stx)
       (#:no-wrap)
       [((? Prim#%App)) ($1 e1 e2 rs tagged-stx)]
       [((? Prim#%Datum)) ($1 e1 e2 rs tagged-stx)]
       [((? Prim#%Top)) ($1 e1 e2 rs tagged-stx)])

      ;; Modules
      (PrimModule
       (#:args e1 e2 rs)
       ;; Multiple forms after language
       ;; #%module-begin tagging done automatically
       [(prim-module ! (? Eval) (? EE 'body))
        (make-p:module e1 e2 rs #f $4)]

       ;; One form after language ... macro that expands into #%module-begin
       [(prim-module NoError next
                     enter-check (? CheckImmediateMacro/Inner) exit-check
                     (! 'module-begin) next (? EE))
        (make-p:module e1 e2 rs 
                       #t
                       ($5 $4 
                           (and (deriv? $9) (deriv-e2 $9))
                           (lambda (ce1 ce2) $9)))])

      (Prim#%ModuleBegin
       (#:args e1 e2 rs)
       [(prim-#%module-begin (! 'malformed)
                             (? ModulePass1 'pass1) next-group 
                             (? ModulePass2 'pass2)
                             (! 'provides))
        (make-p:#%module-begin e1 e2 rs $3 $5)])

      (ModulePass1
       (#:skipped null)
       (#:no-wrap)
       [() null]
       [(next (? ModulePass1-Part) (? ModulePass1))
        (cons $2 $3)]
       [(module-lift-end-loop (? ModulePass1))
        (cons (make-mod:lift-end $1) $2)])

      (ModulePass1-Part
       (#:no-wrap)
       [((? EE) (? ModulePass1/Prim))
        (make-mod:prim $1 $2)]
       [(EE NoError module-lift-loop)
        (make-mod:lift $1 $2)]
       [(EE ! splice)
        (make-mod:splice $1 $3)])

      (ModulePass1/Prim
       [(enter-prim prim-define-values ! exit-prim)
        (make-p:define-values $1 $4 null #f)]
       [(enter-prim prim-define-syntaxes ! phase-up (? EE/LetLifts) (? Eval) exit-prim)
        (make-p:define-syntaxes $1 $7 null $5)]
       [(enter-prim prim-require ! (? Eval) exit-prim)
        (make-p:require $1 $5 null)]
       [(enter-prim prim-require-for-syntax ! (? Eval) exit-prim)
        (make-p:require-for-syntax $1 $5 null)]
       [(enter-prim prim-require-for-template ! (? Eval) exit-prim)
        (make-p:require-for-template $1 $5 null)]
       [(enter-prim prim-provide ! exit-prim)
        (make-p:provide $1 $4 null)]
       [()
        #f])

      (ModulePass2
       (#:skipped null)
       (#:no-wrap)
       [() null]
       [(next (? ModulePass2-Part) (? ModulePass2))
        (cons $2 $3)]
       [(module-lift-end-loop (? ModulePass2))
        (cons (make-mod:lift-end $1) $2)])

      (ModulePass2-Part
       (#:no-wrap)
       ;; not normal; already handled
       [()
        (make-mod:skip)]
       ;; normal: expand completely
       [((? EE))
        (make-mod:cons $1)]
       ;; catch lifts
       [(EE module-lift-loop)
        (make-mod:lift $1 $2)])

      ;; Definitions
      (PrimDefineSyntaxes
       (#:args e1 e2 rs)
       [(prim-define-syntaxes ! (? EE/LetLifts) (? Eval))
        (make-p:define-syntaxes e1 e2 rs $3)])
      
      (PrimDefineValues
       (#:args e1 e2 rs)
       [(prim-define-values ! (? EE))
        (make-p:define-values e1 e2 rs $3)])
      
      ;; Simple expressions
      (PrimExpression
       (#:args e1 e2 rs)
       [(prim-expression ! (? EE 'inner))
        (make-p:expression e1 e2 rs $3)])

      (PrimIf
       (#:args e1 e2 rs)
       [(prim-if ! (? EE 'test) next (? EE 'then) next (? EE 'else))
        (make-p:if e1 e2 rs #t $3 $5 $7)]
       [(prim-if NoError next-group (? EE 'test) next (? EE 'then))
        (make-p:if e1 e2 rs #f $4 $6 #f)])
      
      (PrimWCM 
       (#:args e1 e2 rs)
       [(prim-wcm ! (? EE 'key) next (? EE 'mark) next (? EE 'body))
        (make-p:wcm e1 e2 rs $3 $5 $7)])
      
      ;; Sequence-containing expressions
      (PrimBegin
       (#:args e1 e2 rs)
       [(prim-begin ! (? EL))
        (make-p:begin e1 e2 rs $3)])
      
      (PrimBegin0
       (#:args e1 e2 rs)
       [(prim-begin0 ! next (? EE) next (? EL))
        (make-p:begin0 e1 e2 rs $4 $6)])
      
      (Prim#%App
       (#:args e1 e2 rs tagged-stx)
       [(prim-#%app !)
        (make-p:#%app e1 e2 rs tagged-stx (make-lderiv null null null))]
       [(prim-#%app NoError (? EL))
        (make-p:#%app e1 e2 rs tagged-stx $3)])
      
      ;; Binding expressions
      (PrimLambda
       (#:args e1 e2 rs)
       [(prim-lambda ! renames-lambda (? EB))
        (make-p:lambda e1 e2 rs $3 $4)])
      
      (PrimCaseLambda
       (#:args e1 e2 rs)
       [(prim-case-lambda ! (? NextCaseLambdaClauses))
        (make-p:case-lambda e1 e2 rs $3)])
      
      (NextCaseLambdaClauses
       (#:skipped null)
       [(next ! renames-case-lambda (? EB 'first) (? NextCaseLambdaClauses 'rest))
        (cons (cons $3 $4) $5)]
       [() null])
      
      (PrimLetValues
       (#:args e1 e2 rs)
       [(prim-let-values ! renames-let (? NextEEs 'rhss) next-group (? EB 'body))
        (make-p:let-values e1 e2 rs $3 $4 $6)])
      
      (PrimLet*Values
       (#:args e1 e2 rs)
       ;; let*-values with bindings is "macro-like"
       [(prim-let*-values ! (? EE))
        (let ([next-e1 (lift/deriv-e1 $3)])
          (make-mrule e1 e2 (make-transformation e1 next-e1 rs e1 next-e1 null (new-sequence-number)) $3))]
       ;; No bindings... model as "let"
       [(prim-let*-values NoError renames-let (? NextEEs 'rhss) next-group (? EB 'body))
        (make-p:let-values e1 e2 rs $3 $4 $6)])

      (PrimLetrecValues
       (#:args e1 e2 rs)
       [(prim-letrec-values ! renames-let (? NextEEs 'rhss) next-group (? EB 'body))
        (make-p:letrec-values e1 e2 rs $3 $4 $6)])
      
      (PrimLetrecSyntaxes+Values
       (#:args e1 e2 rs)
       [(prim-letrec-syntaxes+values (! 'bad-syntax) renames-letrec-syntaxes
         (? NextBindSyntaxess 'srhss) next-group (? EB 'body))
        (make-p:letrec-syntaxes+values e1 e2 rs $3 $4 #f null $6)]
       [(prim-letrec-syntaxes+values NoError renames-letrec-syntaxes
         NextBindSyntaxess next-group
         prim-letrec-values (! 'impossible?)
         renames-let (? NextEEs 'vrhss) next-group (? EB 'body))
        (make-p:letrec-syntaxes+values e1 e2 rs $3 $4 $8 $9 $11)])
      
      ;; Atomic expressions
      (Prim#%Datum
       (#:args e1 e2 rs tagged-stx)
       [(prim-#%datum !) (make-p:#%datum e1 e2 rs tagged-stx)])

      (Prim#%Top
       (#:args e1 e2 rs tagged-stx)
       [(prim-#%top !) (make-p:#%top e1 e2 rs tagged-stx)])

      (PrimSTOP
       (#:args e1 e2 rs)
       [(prim-stop !) (make-p:stop e1 e2 rs)])
      
      (PrimQuote
       (#:args e1 e2 rs)
       [(prim-quote !) (make-p:quote e1 e2 rs)])
      
      (PrimQuoteSyntax
       (#:args e1 e2 rs)
       [(prim-quote-syntax !) (make-p:quote-syntax e1 e2 rs)])
      
      (PrimRequire
       (#:args e1 e2 rs)
       [(prim-require ! (? Eval))
        (make-p:require e1 e2 rs)])
      
      (PrimRequireForSyntax
       (#:args e1 e2 rs)
       [(prim-require-for-syntax ! (? Eval))
        (make-p:require-for-syntax e1 e2 rs)])
      
      (PrimRequireForTemplate
       (#:args e1 e2 rs)
       [(prim-require-for-template ! (? Eval))
        (make-p:require-for-template e1 e2 rs)])
      
      (PrimProvide 
       (#:args e1 e2 rs)
       [(prim-provide !) (make-p:provide e1 e2 rs)])
      
      (PrimSet
       (#:args e1 e2 rs)
       [(prim-set! ! Resolves next (? EE))
        (make-p:set! e1 e2 rs $3 $5)]
       [(prim-set! NoError (? MacroStep 'macro) (? EE 'continue))
        (make-p:set!-macro e1 e2 rs (make-mrule e1 (and (deriv? $4) (deriv-e2 $4)) $3 $4))])
      
      ;; Blocks
      ;; EB Answer = BlockDerivation
      (EB 
       [(enter-block (? BlockPass1 'pass1) block->list (? EL 'pass2))
        (make-bderiv $1
                     (and (lderiv? $4) (lderiv-es2 $4))
                     $2
                     'list
                     $4)]
       [(enter-block BlockPass1 block->letrec (? EL 'pass2))
        (make-bderiv $1
                     (and (lderiv? $4) (lderiv-es2 $4))
                     $2
                     'letrec
                     $4)])

      ;; BlockPass1 Answer = (list-of BRule)
      (BlockPass1
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [((? BRule) (? BlockPass1))
        (cons $1 $2)])

      ;; BRule Answer = BRule
      (BRule
       [(next ! IMPOSSIBLE)
        #f]
       [(next NoError renames-block (? CheckImmediateMacro 'check))
        (make-b:expr $3 $4)]
       [(next NoError renames-block CheckImmediateMacro prim-begin ! splice !)
        (make-b:splice $3 $4 $7)]
       [(next NoError renames-block CheckImmediateMacro prim-define-values !)
        (make-b:defvals $3 $4)]
       [(next NoError renames-block CheckImmediateMacro
              prim-define-syntaxes (? BindSyntaxes 'bind))
        (make-b:defstx $3 $4 $6)])

      ;; BindSyntaxes Answer = Derivation
      (BindSyntaxes
       [(phase-up (? EE/LetLifts) ! (? Eval)) $2])
      
      ;; NextBindSyntaxess Answer = (list-of Derivation)
      (NextBindSyntaxess
       (#:skipped null)
       (#:no-wrap)
       [() null]
       [(next (? BindSyntaxes 'first) (? NextBindSyntaxess 'rest)) (cons $2 $3)])
      
      ;; Lists
      ;; EL Answer = ListDerivation
      (EL
       (#:skipped #f)
       [(enter-list ! (? EL*) exit-list) (make-lderiv $1 $4 $3)])

      ;; EL* Answer = (listof Derivation)
      (EL*
       (#:no-wrap)
       (#:skipped null)
       [() null]
       [(next (? EE 'first) (? EL* 'rest)) (cons $2 $3)])
      
      )))
  
  )
