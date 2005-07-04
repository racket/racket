(module parameters mzscheme
  
  (provide (all-defined))
  
  (define current-compile-context (make-parameter #f))
  
  ;; tenv and lenv, respectively
  (define current-type-environment    (make-parameter #f))
  (define current-lexical-environment (make-parameter #f))
  
  )