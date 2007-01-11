
(module steps mzscheme
  (provide (all-defined))

  ;; A ReductionSequence is a (list-of Reduction)

  ;; A ProtoStep is (make-protostep Derivation BigContext StepType)
  
  ;; A Reduction is one of 
  ;;   - (make-step ... Syntaxes Syntaxes Syntax Syntax)
  ;;   - (make-misstep ... Syntax Syntax Exception)
  
  (define-struct protostep (deriv lctx type) #f)
  
  (define-struct (step protostep) (redex contractum e1 e2) #f)
  (define-struct (misstep protostep) (redex e1 exn) #f)
  
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
