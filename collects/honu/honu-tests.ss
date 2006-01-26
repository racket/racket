(module honu-tests mzscheme

  (require (lib "contract.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "private/tests/typechecker-tests.ss"
           "private/tests/program-tests.ss"
           )

  (provide/contract [honu-tests test-suite?])

  ;; honu-tests : TestSuite
  ;; Honu Test Suite
  (define honu-tests
    (make-test-suite
     "Honu"
     program-tests
     typechecker-tests
     ))
  
  )
