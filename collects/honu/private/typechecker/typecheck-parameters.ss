(module typecheck-parameters mzscheme

  (provide (all-defined))
  
  ;; since the class-environment doesn't change within typecheck-expression, we make it a parameter
  ;; that we set before calling it.
  ;;
  ;; cenv  : ((syntax/c symbol?) . -> . (union honu:type false/c))
  ;;   static environment inside of a class or mixin definition
  ;;   (i.e. for my.<id>)

  (define current-class-environment (make-parameter (lambda (name) #f)))
  
  ;; since the return type only changes when we go into a lambda in typecheck-expression, we
  ;; make it a parameter also.
  ;;
  ;; rtype : (union honu:type? false/c)
  ;;   return type for method/function

  (define current-return-type (make-parameter #f))
  
  )