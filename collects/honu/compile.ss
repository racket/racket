(module compile mzscheme
 
  (require (lib "contract.ss"))
 
  (require "ast.ss")
  (require "tenv.ss")
  (require "tenv-utils.ss")
  (require "honu-compile-context.ss")
  (require "private/compiler/honu-translate-utils.ss")
  (require "private/compiler/honu-translate-program.ss")
  (require "private/compiler/honu-translate-expression.ss")
  (require "private/typechecker/honu-typecheck.ss")
  (require "private/typechecker/honu-typecheck-exp.ss")
  (require "read-error-with-stx.ss")
  
  (provide/contract [compile/complete-program
                     (tenv? honu-program?
                      . -> . 
;                     (listof (syntax/c any/c))]
                      list?)]
                    [compile/interaction
                     ((tenv? 
                       any/c
                       (union honu-binding? honu-exp?))
                      . ->* . 
;                     (listof (syntax/c any/c))]
                      (any/c any/c))])
  (define (compile/complete-program tenv pgm)
    (add-defns-to-tenv (honu-program-defns pgm) tenv)
    (let ([checked (honu-typecheck-program tenv pgm)])
      (parameterize ([current-compile-context honu-compile-context])
        (honu-translate-program tenv checked))))
  
  (define (compile/interaction tenv env ast)
    (cond
      [(honu-binding? ast)
       (if (env (honu-binding-name ast))
           (raise-read-error-with-stx
            (format "~a already bound" (printable-key (honu-binding-name ast)))
            (honu-binding-name ast))
           (let-values ([(checked new-env)
                         ((honu-typecheck-binding tenv #f) ast env)])
             (parameterize ([current-compile-context honu-compile-context])
               (values (honu-translate-binding tenv #f checked #t)
                       new-env))))]
      [(honu-exp? ast)
       (let-values ([(checked type) ((honu-typecheck-exp tenv env #f) ast #f)])
         (parameterize ([current-compile-context honu-compile-context])
           (values (honu-translate-expression tenv #f checked)
                   env)))]))
  )
    
