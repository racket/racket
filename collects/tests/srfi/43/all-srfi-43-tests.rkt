(module all-srfi-43-tests mzscheme
  (require rackunit)
  (require "constructor-tests.ss"
           "predicate-tests.ss"
           "iteration-tests.ss"
           "searching-tests.ss"
           "mutator-tests.ss"
           "conversion-tests.ss")
  (provide all-srfi-43-tests)
  
  (define all-srfi-43-tests
    (test-suite 
     "all-tests-tests"
     constructor-tests
     predicate-tests
     iteration-tests
     searching-tests
     mutator-tests
     conversion-tests)))
