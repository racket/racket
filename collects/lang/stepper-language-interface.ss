(module stepper-language-interface mzscheme
  
  (require mzlib/class)
  (provide stepper-language<%>)
    
  (define stepper-language<%>
    (interface ()
      stepper:supported?
      stepper:enable-let-lifting?
      stepper:show-lambdas-as-lambdas?
      stepper:show-inexactness?
      stepper:show-consumed-and/or-clauses?
      stepper:render-to-sexp)))
