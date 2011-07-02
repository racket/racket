(module all-srfi-43-tests mzscheme
  (require rackunit)
  (require "constructor-tests.rkt"
           "predicate-tests.rkt"
           "iteration-tests.rkt"
           "searching-tests.rkt"
           "mutator-tests.rkt"
           "conversion-tests.rkt")
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
