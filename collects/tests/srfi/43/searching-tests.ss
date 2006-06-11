(module searching-tests mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
  (require (lib "searching.ss" "srfi" "43"))
  
  (provide searching-tests)
  
  (define searching-tests
    (make-test-suite
     "All tests for searching"
     (make-test-case
      "vector-index"
      (assert =
              (vector-index even? '#(3 1 4 1 5 9))
              2)
      (assert =
              (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              1)
      (assert-false
       (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))
     
     (make-test-case
      "vector-index-right"
      (assert =
              (vector-index-right even? '#(3 1 4 1 5 9))
              2)
      (assert =
              (vector-index-right < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              3)
      (assert-false
       (vector-index-right = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))
     
     (make-test-case
      "vector-skip"
      (assert =
              (vector-skip odd? '#(3 1 4 1 5 9))
              2)
      (assert =
              (vector-skip > '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              1)
      (assert-false
       (vector-skip = '#(2 7 1 8 2 8) '#(2 7 1 8 2))))
     
     (make-test-case
      "vector-skip-right"
      (assert =
              (vector-skip-right odd? '#(3 1 4 1 5 9))
              2)
      (assert =
              (vector-skip-right > '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
              3)
      (assert-false
       (vector-skip-right = '#(2 7 1 8 2 8) '#(2 7 1 8 2))))
     
     (make-test-case
      "vector-binary-search"
      (assert =
              (vector-binary-search '#(0 3 4 6 8 9) 0 -)
              0)
      (assert =
              (vector-binary-search '#(0 3 4 6 8 9) 3 -)
              1)
      (assert =
              (vector-binary-search '#(0 3 4 6 8 9) 4 -)
              2)
      (assert =
              (vector-binary-search '#(0 3 4 6 8 9) 6 -)
              3)
      (assert =
              (vector-binary-search '#(0 3 4 6 8 9) 8 -)
              4)
      (assert =
              (vector-binary-search '#(0 3 4 6 8 9) 9 -)
              5)
      (assert-false
       (vector-binary-search '#(0 3 4 6 8 9) 5 -))
      (assert-false
       (vector-binary-search '#(0 3 4 6 8 9) -2 -))
      (assert-false
       (vector-binary-search '#(0 3 4 6 8 9) 11 -))
      (assert-false
       (vector-binary-search '#(0 3 4 6 8 9) 1 -)))
     
     (make-test-case
      "vector-any"
      (assert-false
       (vector-any zero? '#(1 2 3 4)))
      (assert-true
       (vector-any zero? '#(2 0 1)))
      (assert =
              (vector-any / '#(1 1) '#(1 0))
              1)
      (assert-false
       (vector-any (lambda x #t) '#())))
     
     (make-test-case
      "vector-every"
      (assert-false
       (vector-every zero? '#(0 0 7)))
      (assert-true
       (vector-every (lambda x #f) '#()))
      (assert =
              (vector-every - '#(1 1) '#(1 0))
              1))
     ))
  )
