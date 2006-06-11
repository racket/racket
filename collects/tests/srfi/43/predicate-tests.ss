(module predicate-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
  (require (lib "predicates.ss" "srfi" "43"))

  (provide predicate-tests)

  (define predicate-tests
    (make-test-suite
     "All tests for predicate"
     (make-test-case
      "vector-empty?"
      (assert-false
       (vector-empty? '#(a)))
      (assert-false
       (vector-empty? '#(())))
      (assert-false
       (vector-empty? '#(#())))
      (assert-true
       (vector-empty? '#())))

     (make-test-case
      "vector="
      (assert-true
       (vector= eq? '#(a b c d) '#(a b c d)))
      (assert-false
       (vector= eq? '#(a b c d) '#(a b d c)))
      (assert-false
       (vector= = '#(1 2 3 4 5) '#(1 2 3 4)))
      (assert-true
       (vector= = '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))
      (assert-true
       (vector= eq?))
      (assert-true
       (vector= eq? '#(a)))
      (assert-false
       (vector= eq? '#(a b c d) '#(a b c d) '#(a b d c)))
      (assert-false
       (vector= eq? '#(a b c d e) '#(a b c d) '#(a b c d))))
     ))
  )
     
