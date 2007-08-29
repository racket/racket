
(module steps mzscheme
  (require "deriv.ss"
           "deriv-util.ss")
  (provide (all-defined))

  ;; A ReductionSequence is a (list-of Reduction)

  ;; A ProtoStep is (make-protostep Derivation BigContext StepType Context Definites)

  ;; A Context is a list of Frames
  ;; A Frame is either:
  ;;  - (syntax -> syntax)
  ;;  - (make-renames syntax syntax)
  ;;  - 'phase-up
  (define-struct renames (old new))

  ;; A Definite is a (list-of identifier)

  ;; A BigContext is (list-of BigFrame)
  ;; A BigFrame is (make-bigframe Derivation Context Syntaxes Syntax)
  (define-struct bigframe (deriv ctx foci e))

  ;; A Reduction is one of 
  ;;   - (make-step ... Syntaxes Syntaxes Syntax Syntax)
  ;;   - (make-mono ... Syntaxes Syntax)
  ;;   - (make-misstep ... Syntax Syntax Exception)

  (define-struct protostep (deriv lctx type ctx definites frontier) #f)

  (define-struct (step protostep) (foci1 foci2 e1 e2) #f)
  (define-struct (mono protostep) (foci1 e1) #f)
  (define-struct (misstep protostep) (foci1 e1 exn) #f)

  ;; context-fill : Context Syntax -> Syntax
  (define (context-fill ctx stx)
    (let loop ([ctx ctx] [stx stx])
      (if (null? ctx)
          stx
          (let ([frame0 (car ctx)])
            (if (procedure? frame0)
                (loop (cdr ctx) (frame0 stx))
                (loop (cdr ctx) stx))))))

  ;; context-env : Context -> (list-of identifier)
  (define (context-env ctx)
    (let loop ([ctx ctx] [env null])
      (if (null? ctx)
          env
          (let ([frame0 (car ctx)])
            (if (renames? frame0)
                (loop (cdr ctx)
                      (append (flatten-identifiers (renames-new frame0))
                              env))
                (loop (cdr ctx) env))))))

  (define (step-term1 s)
    (context-fill (protostep-ctx s) (step-e1 s)))
  (define (step-term2 s)
    (context-fill (protostep-ctx s) (step-e2 s)))

  (define (mono-term1 s)
    (context-fill (protostep-ctx s) (mono-e1 s)))

  (define (misstep-term1 s)
    (context-fill (protostep-ctx s) (misstep-e1 s)))

  (define (bigframe-term bf)
    (context-fill (bigframe-ctx bf) (bigframe-e bf)))

  ;; A StepType is a simple in the following alist.

  (define step-type-meanings
    '((macro-step       . "Macro transformation")
      
      (rename-lambda    . "Rename formal parameters")
      (rename-case-lambda . "Rename formal parameters")
      (rename-let-values . "Rename bound variables")
      (rename-letrec-values . "Rename bound variables")
      (rename-lsv       . "Rename bound variables")
      (lsv-remove-syntax . "Remove syntax bindings")

      (resolve-variable . "Resolve variable (remove extra marks)")
      (tag-module-begin . "Tag #%module-begin")
      (tag-app          . "Tag application")
      (tag-datum        . "Tag datum")
      (tag-top          . "Tag top-level variable")
      (capture-lifts    . "Capture lifts")

      (local-lift       . "Macro lifted expression to top-level")
      (module-lift      . "Macro lifted declaration to end of module")
      (block->letrec    . "Transform block to letrec")
      (splice-block     . "Splice block-level begin")
      (splice-module    . "Splice module-level begin")
      (splice-lifts     . "Splice definitions from lifted expressions")
      (splice-module-lifts . "Splice lifted module declarations")

      (error            . "Error")))

  (define (step-type->string x)
    (cond [(assq x step-type-meanings) => cdr]
          [(string? x) x]
          [else (error 'step-type->string "not a step type: ~s" x)]))

  (define (rename-step? x)
    (memq (protostep-type x) 
          '(rename-lambda
            rename-case-lambda
            rename-let-values
            rename-letrec-values
            rename-lsv)))

  (define (rewrite-step? x)
    (and (step? x) (not (rename-step? x))))
)
