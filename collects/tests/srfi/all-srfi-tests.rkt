(module all-srfi-tests mzscheme
  
  (require rackunit)
  (require "1/all-1-tests.ss"
           "2/and-let-test.ss"
           "4/srfi-4-test.ss"
           "13/string-test.ss"
           "14/char-set-test.ss"
           "26/cut-test.ss"
           "40/all-srfi-40-tests.ss"
           "43/all-srfi-43-tests.ss"
           "69/hash-tests.ss")
  (provide all-srfi-tests)
  
  (define all-srfi-tests
    (test-suite 
     "all-srfi-tests"
     all-1-tests
     and-let*-tests
     string-tests
     char-set-tests
     cut-tests
     all-srfi-40-tests
     all-srfi-43-tests
     hash-tests
     srfi-4-tests
     ))
  )
