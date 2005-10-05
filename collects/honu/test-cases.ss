(module test-cases mzscheme

  (require (lib "contract.ss")
           (prefix srfi13: (lib "13.ss" "srfi"))
           "private/tools/test.ss"
           "private/typechecker/typecheck-expression.ss"
           "tenv.ss"
           "ast.ss"
           "utils.ss"
           "top.ss")
  
  (define/p examples
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

  (define-test-suite honu-tests
    (test-case
     examples-simple
     (map test-file examples)
     [pred: (lambda (all-results)
              (andmap (lambda (file-results)
                        (andmap (lambda (result) (eq? result #t))
                                file-results))
                      all-results))])
    (test-suite typechecker
       (test-suite expression
          (test-case sequence-not-void
                     (typecheck-expression
                      (wrap-lenv) #f
                      (make-honu:seq
                       #'()
                       (list (make-honu:lit #'() (make-honu:type-prim #'() 'int) 5))
                       (make-honu:lit #'() (make-honu:type-prim #'() 'int) 4)))
                     [error: (lambda (exn) (srfi13:string-contains (exn-message exn) "void"))]))))
  
  (define/c (run-tests) (-> report?)
    (honu-tests))
  
  )