(module predicate-tests mzscheme

  (require rackunit)
  (require srfi/43/vector-lib)

  (provide predicate-tests)

  (define predicate-tests
    (test-suite
     "All tests for predicate"
     (test-case
      "vector-empty?"
      (check-false
       (vector-empty? '#(a)))
      (check-false
       (vector-empty? '#(())))
      (check-false
       (vector-empty? '#(#())))
      (check-true
       (vector-empty? '#())))

     (test-case
      "vector="
      (check-true
       (vector= eq? '#(a b c d) '#(a b c d)))
      (check-false
       (vector= eq? '#(a b c d) '#(a b d c)))
      (check-false
       (vector= = '#(1 2 3 4 5) '#(1 2 3 4)))
      (check-true
       (vector= = '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))
      (check-true
       (vector= eq?))
      (check-true
       (vector= eq? '#(a)))
      (check-false
       (vector= eq? '#(a b c d) '#(a b c d) '#(a b d c)))
      (check-false
       (vector= eq? '#(a b c d e) '#(a b c d) '#(a b c d))))
     ))
  )
     
