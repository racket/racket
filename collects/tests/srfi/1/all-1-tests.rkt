(module all-1-tests mzscheme
  
  (require racunit)
  (require "alist-test.ss"
           "cons-test.ss"
           "delete-test.ss"
           "filter-test.ss"
           "fold-test.ss"
           "lset-test.ss"
           "misc-test.ss"
           "predicate-test.ss"
           "search-test.ss"
           "selector-test.ss")
  
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
