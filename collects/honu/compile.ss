(module compile mzscheme
 
  (require (lib "boundmap.ss" "syntax")
           (lib "contract.ss")
           (lib "plt-match.ss")
           "ast.ss"
           "honu-context.ss"
           "readerr.ss"
           "tenv.ss"
           "tenv-utils.ss"
           "parsers/post-parsing.ss"
           "private/compiler/translate.ss"
           "private/compiler/translate-expression.ss"
           "private/compiler/translate-utils.ss"
           "private/typechecker/type-utils.ss"
           "private/typechecker/typechecker.ss"
           "private/typechecker/typecheck-expression.ss")
  
  (provide/contract [compile/defns
                     (tenv? tenv? (listof honu:defn?)
                      . -> . 
                      (listof (syntax/c any/c)))]
                    [compile/interaction
                     ((tenv? 
                       tenv?
                       (union honu:bind-top? honu:expr?))
                      . ->* . 
                      ((syntax/c any/c)
                       (union honu:type? false/c)))])
  (define (compile/defns tenv lenv pgm)
    (let ([pgm (post-parse-program tenv (add-defns-to-tenv pgm tenv))])
      (let ([checked (typecheck tenv lenv pgm)])
        (parameterize ([current-compile-context honu-compile-context])
          (translate tenv checked)))))
  
  (define (check-bound-names lenv names)
    (for-each (lambda (n)
                (if (and n (bound-identifier-mapping-get lenv n (lambda () #f)))
                    (raise-read-error-with-stx
                     (format "~a already bound" (printable-key n))
                     n)))
              names))
  
  (define (compile/interaction tenv lenv ast)
    (match (post-parse-interaction tenv ast)
      [(struct honu:bind-top (stx names _ value))
       (check-bound-names lenv names)
       (let ([checked (typecheck-defn tenv lenv ast)])
         (parameterize ([current-compile-context honu-compile-context])
           (values (translate-defn tenv checked) #f)))]
      [else
       (let-values ([(checked type) (typecheck-expression tenv (lambda (n) #f)
                                                          (wrap-as-function lenv) (make-top-type #f) #f ast)])
         (parameterize ([current-compile-context honu-compile-context])
           (values (translate-expression tenv #f checked) type)))]))
  )
    
