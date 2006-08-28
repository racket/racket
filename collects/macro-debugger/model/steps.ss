
(module steps mzscheme
  (provide (all-defined))

  ;; A ReductionSequence is a (list-of Reduction)
  
  ;; A Reduction is one of 
  ;;   - (make-step Syntaxes Syntaxes Syntax Syntax BigContext)
  ;;   - (make-misstep Syntax Syntax Exception)
  (define-struct step (redex contractum e1 e2 note lctx) #f)
  (define-struct misstep (redex e1 exn) #f)

  (define-struct (rewrite-step step) () #f)
  (define-struct (rename-step step) () #f)
  
  )
