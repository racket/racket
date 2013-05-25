(module searching-tests mzscheme
  
  (require rackunit)
  (require srfi/43/vector-lib)
  
  (provide searching-tests)
  
  (define searching-tests
    (test-suite
     "All tests for searching"
     (test-case
      "vector-index"
      (check =
              (vector-index even? '#(3 1 4 1 5 9))
              2)
      (check =
              (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              1)
      (check-false
       (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))
     
     (test-case
      "vector-index-right"
      (check =
              (vector-index-right even? '#(3 1 4 1 5 9))
              2)
      (check =
              (vector-index-right < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              3)
      (check-false
       (vector-index-right = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))
     
     (test-case
      "vector-skip"
      (check =
              (vector-skip odd? '#(3 1 4 1 5 9))
              2)
      (check =
              (vector-skip > '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              1)
      (check-false
       (vector-skip = '#(2 7 1 8 2 8) '#(2 7 1 8 2))))
     
     (test-case
      "vector-skip-right"
      (check =
              (vector-skip-right odd? '#(3 1 4 1 5 9))
              2)
      (check =
              (vector-skip-right > '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              3)
      (check-false
       (vector-skip-right = '#(2 7 1 8 2 8) '#(2 7 1 8 2))))
     
     (test-case
      "vector-binary-search"
      (check =
              (vector-binary-search '#(0 3 4 6 8 9) 0 -)
              0)
      (check =
              (vector-binary-search '#(0 3 4 6 8 9) 3 -)
              1)
      (check =
              (vector-binary-search '#(0 3 4 6 8 9) 4 -)
              2)
      (check =
              (vector-binary-search '#(0 3 4 6 8 9) 6 -)
              3)
      (check =
              (vector-binary-search '#(0 3 4 6 8 9) 8 -)
              4)
      (check =
              (vector-binary-search '#(0 3 4 6 8 9) 9 -)
              5)
      (check-false
       (vector-binary-search '#(0 3 4 6 8 9) 5 -))
      (check-false
       (vector-binary-search '#(0 3 4 6 8 9) -2 -))
      (check-false
       (vector-binary-search '#(0 3 4 6 8 9) 11 -))
      (check-false
       (vector-binary-search '#(0 3 4 6 8 9) 1 -)))
     
     (test-case
      "vector-any"
      (check-false
       (vector-any zero? '#(1 2 3 4)))
      (check-true
       (vector-any zero? '#(2 0 1)))
      (check =
              (vector-any / '#(1 1) '#(1 0))
              1)
      (check-false
       (vector-any (lambda x #t) '#())))
     
     (test-case
      "vector-every"
      (check-false
       (vector-every zero? '#(0 0 7)))
      (check-true
       (vector-every (lambda x #f) '#()))
      (check =
              (vector-every - '#(1 1) '#(1 0))
              1))
     ))
  )
