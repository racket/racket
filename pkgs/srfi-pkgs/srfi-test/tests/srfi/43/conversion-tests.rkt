(module conversion-tests mzscheme
  
  (require rackunit)
  (require srfi/43/vector-lib)
  
  (provide conversion-tests)
  
  (define conversion-tests
    (test-suite
     "All tests for conversion"
     (test-case
      "vector->list"
      (check-equal?
       (s:vector->list '#(1 2 3 4))
       '(1 2 3 4))
      (check-equal?
       (s:vector->list '#(1 2 3 4) 4)
       '())
      (check-equal?
       (s:vector->list '#(1 2 3 4) 1 3)
       '(2 3)))
     
     (test-case
      "reverse-vector->list"
      (check-equal?
       (reverse-vector->list '#(1 2 3 4))
       '(4 3 2 1))
      (check-equal?
       (reverse-vector->list '#(1 2 3 4) 4)
       '())
      (check-equal?
       (reverse-vector->list '#(1 2 3 4) 1 3)
       '(3 2)))
     
     (test-case
      "reverse-list->vector"
      (check-equal?
       (reverse-list->vector '(1 2 3 4))
       '#(4 3 2 1))
      (check-equal?
       (reverse-list->vector '())
       '#()))
     ))
  )
