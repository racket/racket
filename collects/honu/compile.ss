(module compile mzscheme
 
  (require (lib "boundmap.ss" "syntax")
           (lib "contract.ss")
           (lib "plt-match.ss")
           "ast.ss"
           "honu-context.ss"
           "parameters.ss"
           "readerr.ss"
           "tenv.ss"
           "tenv-utils.ss"
           "parsers/post-parsing.ss"
           "private/compiler/translate.ss"
           "private/compiler/translate-expression.ss"
           "private/typechecker/type-utils.ss"
           "private/typechecker/typechecker.ss"
           "private/typechecker/typecheck-expression.ss")
  
  (provide/contract [compile/defns
                     ((tenv? tenv? (listof ast:defn?))
                      . ->* . 
                      (any/c (listof (syntax/c any/c))))]
                    [compile/interaction
                     ((tenv? 
                       tenv?
                       (union ast:defn:binding? ast:expr?))
                      . ->* . 
                      ((syntax/c any/c)
                       (union ast:type? false/c)))])
  (define (compile/defns tenv lenv pgm)
    (parameterize ([current-type-environment    tenv]
                   [current-lexical-environment lenv])
      (let ([pgm (post-parse-program (add-defns-to-tenv pgm))])
        (let ([checked (typecheck pgm)])
          (parameterize ([current-compile-context honu-compile-context])
            (translate checked))))))
  
  (define (check-bound-names names)
    (for-each (lambda (n)
                (if (and n (get-lenv-entry n))
                    (raise-read-error-with-stx
                     (format "~a already bound" (printable-key n))
                     n)))
              names))
  
  (define (compile/interaction tenv lenv ast)
    (parameterize ([current-type-environment    tenv]
                   [current-lexical-environment lenv])
      (match (post-parse-interaction ast)
        [(struct ast:defn:binding (stx names _ value))
         (check-bound-names names)
         (let ([checked (typecheck-defn ast)])
           (parameterize ([current-compile-context honu-compile-context])
             (values (translate-defn checked) #f)))]
        [else
         (let-values ([(checked type)
                       (typecheck-expression
                        (wrap-lenv) (make-top-type #f) ast)])
           (parameterize ([current-compile-context honu-compile-context])
             (values (translate-expression checked) type)))])))
  )
    
