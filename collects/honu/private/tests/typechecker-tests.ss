(module typechecker-tests mzscheme

  (require (lib "contract.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (prefix srfi13: (lib "13.ss" "srfi"))
           "../typechecker/typecheck-expression.ss"
           "../../tenv.ss"
           "../../ast.ss"
           )

  (provide/contract [typechecker-tests test-suite?])

  (define non-void-in-sequence-test
    (make-test-case "Non-void expression in a sequence"
                    (assert-exn
                     (lambda (exn)
                       (srfi13:string-contains (exn-message exn) "void"))
                     (lambda ()
                       (typecheck-expression
                        (wrap-lenv) #f
                        (make-ast:expr:sequence
                         #'()
                         (list (make-ast:expr:literal #'() (make-ast:type:primitive #'() 'int) #'5))
                         (make-ast:expr:literal #'() (make-ast:type:primitive #'() 'int) #'4)))))))

  (define error-message-tests
    (make-test-suite
     "Error messages"
     non-void-in-sequence-test
     ))

  (define typechecker-tests
    (make-test-suite
     "Typechecker"
     error-message-tests
     ))

  )