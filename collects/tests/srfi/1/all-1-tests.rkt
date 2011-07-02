(module all-1-tests mzscheme
  
  (require rackunit)
  (require "alist-test.rkt"
           "cons-test.rkt"
           "delete-test.rkt"
           "filter-test.rkt"
           "fold-test.rkt"
           "lset-test.rkt"
           "misc-test.rkt"
           "predicate-test.rkt"
           "search-test.rkt"
           "selector-test.rkt")
  
  (provide all-1-tests)
  
  (define all-1-tests
    (test-suite 
     "all-1-tests"
     alist-tests
     cons-tests
     delete-tests
     filter-tests
     fold-tests
     lset-tests
     misc-tests
     predicate-tests
     search-tests
     selector-tests
     ))
  )
