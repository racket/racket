(module test-cases mzscheme

  (require (lib "contract.ss")
           (prefix srfi13: (lib "13.ss" "srfi"))
           "private/tools/test.ss"
           "private/typechecker/typecheck-expression.ss"
           "tenv.ss"
           "ast.ss"
           "top.ss")
  
  (define example-files
    (list "examples/BoundedStack.honu"
          "examples/EvenOddClass.honu"
          "examples/List.honu"
          "examples/Y.honu"
          "examples/bind-tup-top.honu"
          "examples/cond-test.honu"
          "examples/even-odd.honu"
          "examples/exprs.honu"
          "examples/point.honu"
          "examples/struct.honu"
          "examples/tup-bind.honu"
;          "examples/types-error.honu"
          "examples/types.honu"
;          "examples/nonexistent.honu"
          ))

  (provide void-example example-files)
  
  (define (void-example)
    (typecheck-expression
     (wrap-lenv) #f
     (make-ast:expr:sequence
      #'()
      (list (make-ast:expr:literal #'() (make-ast:type:primitive #'() 'int) #'5))
      (make-ast:expr:literal #'() (make-ast:type:primitive #'() 'int) #'4))))
  
  (define-test-suite honu-tests
    (test-case
     examples-simple
     (map test-file example-files)
     [pred: (lambda (all-results)
              (andmap (lambda (file-results)
                        (andmap (lambda (result) (eq? result #t))
                                file-results))
                      all-results))])
    (test-suite typechecker
       (test-suite expression
          (test-case sequence-not-void
                     (void-example)
                     [error: (lambda (exn) (srfi13:string-contains (exn-message exn) "void"))]))))

  (provide/contract
   [run-tests (-> report?)]
   [run-examples (-> void?)]
   )
  
  (define (run-tests)
    (honu-tests))

  (define (run-examples)
    (for-each run-program example-files))
  
  )