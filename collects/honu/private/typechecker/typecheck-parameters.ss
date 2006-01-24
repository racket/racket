(module typecheck-parameters mzscheme

  (provide (all-defined))
  
  ;; since the return type only changes when we go into a lambda in typecheck-expression, we
  ;; make it a parameter also.
  ;;
  ;; rtype : (union ast:type? false/c)
  ;;   return type for method/function

  (define current-return-type (make-parameter #f))
  
  )
