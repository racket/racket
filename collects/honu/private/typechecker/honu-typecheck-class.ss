(module honu-typecheck-class mzscheme
  
  (require (lib "list.ss" "srfi" "1"))
  (require (lib "struct.ss"))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "honu-type-utils.ss")
  (require "honu-typecheck-class-utils.ss")
  
  (provide honu-typecheck-class)
  (define (honu-typecheck-class tenv cls)
    (check-impl-types tenv (honu-class-impls cls))
    (check-init-slots tenv (honu-class-init-names cls) (honu-class-init-types cls))
    (let-values (((new-defns new-env new-cenv new-init-cenv)
                  (honu-typecheck-slotdefns tenv
                                            (extend-env (get-initial-env tenv) #'this (honu-class-type cls))
                                            (empty-env)
                                            (fold (lambda (n t e)
                                                    (extend-env e n t))
                                                  (empty-env)
                                                  (honu-class-init-names cls)
                                                  (honu-class-init-types cls))
                                            (honu-class-defns cls))))
      (check-impls-and-exports tenv new-cenv
                               (honu-class-type cls)
                               (honu-class-impls cls)
                               (honu-class-exports cls))
      (copy-struct honu-class cls
        (honu-class-defns new-defns))))
  )
