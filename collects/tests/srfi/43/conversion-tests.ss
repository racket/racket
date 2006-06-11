(module conversion-tests mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
  (require (all-except (lib "conversion.ss" "srfi" "43") vector->list)
           (rename (lib "conversion.ss" "srfi" "43") s:vector->list vector->list))
  
  (provide conversion-tests)
  
  (define conversion-tests
    (make-test-suite
     "All tests for conversion"
     (make-test-case
      "vector->list"
      (assert-equal?
       (s:vector->list '#(1 2 3 4))
       '(1 2 3 4))
      (assert-equal?
       (s:vector->list '#(1 2 3 4) 4)
       '())
      (assert-equal?
       (s:vector->list '#(1 2 3 4) 1 3)
       '(2 3)))
     
     (make-test-case
      "reverse-vector->list"
      (assert-equal?
       (reverse-vector->list '#(1 2 3 4))
       '(4 3 2 1))
      (assert-equal?
       (reverse-vector->list '#(1 2 3 4) 4)
       '())
      (assert-equal?
       (reverse-vector->list '#(1 2 3 4) 1 3)
       '(3 2)))
     
     (make-test-case
      "reverse-list->vector"
      (assert-equal?
       (reverse-list->vector '(1 2 3 4))
       '#(4 3 2 1))
      (assert-equal?
       (reverse-list->vector '())
       '#()))
     ))
  )
