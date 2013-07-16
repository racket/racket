(module mutator-tests mzscheme
  
  (require rackunit)
  (require srfi/43/vector-lib)
  
  (provide mutator-tests)
  
  (define mutator-tests
    (test-suite
     "All tests for mutator"
     (test-case
      "vector-swap!"
      (let ((vec (vector 'a 'b 'c 'd 'e)))
        (check-equal?
         (begin (vector-swap! vec 1 3)
                vec)
         '#(a d c b e)))
      (let ((vec (vector 0 1 2)))
        (check-equal?
         (begin (vector-swap! vec 1 1)
                vec)
         '#(0 1 2))))
     
     (test-case
      "vector-fill!"
      (let ((vec (vector 1 2 3 4 5)))
        (check-equal?
         (begin (s:vector-fill! vec 0)
                vec)
         '#(0 0 0 0 0)))
      (let ((vec (vector 1 2 3 4 5)))
        (check-equal?
         (begin (s:vector-fill! vec 0 1)
                vec)
         '#(1 0 0 0 0)))
      (let ((vec (vector 1 2 3 4 5)))
        (check-equal?
         (begin (s:vector-fill! vec 0 1 4)
                vec)
         '#(1 0 0 0 5))))
     
     (test-case
      "vector-reverse!"
      (let ((vec (vector 1 2 3 4 5)))
        (check-equal?
         (begin (vector-reverse! vec)
                vec)
         '#(5 4 3 2 1)))
      (let ((vec (vector 1 2 3 4 5)))
        (check-equal?
         (begin (vector-reverse! vec 1)
                vec)
         '#(1 5 4 3 2)))
      (let ((vec (vector 1 2 3 4 5)))
        (check-equal?
         (begin (vector-reverse! vec 1 4)
                vec)
         '#(1 4 3 2 5))))
     
     (test-case
      "vector-copy!"
      (let ((source (vector 1 2 3 4 5))
            (target (vector 0 0 0 0 0)))
        (check-equal?
         (begin (vector-copy! target 0 source)
                target)
         source))
      (let ((source (vector 1 2 3 4 5))
            (target (vector 0 0 0 0 0)))
        (check-equal?
         (begin (vector-copy! target 1 source 1)
                target)
         '#(0 2 3 4 5)))
      (let ((source (vector 1 2 3 4 5))
            (target (vector 0 0 0 0 0)))
        (check-equal?
         (begin (vector-copy! target 1 source 1 4)
                target)
         '#(0 2 3 4 0))))
     
     (test-case
      "vector-reverse-copy!"
      (let ((source (vector 1 2 3 4 5))
            (target (vector 0 0 0 0 0)))
        (check-equal?
         (begin (vector-reverse-copy! target 0 source)
                target)
         '#(5 4 3 2 1)))
      (let ((source (vector 1 2 3 4 5))
            (target (vector 0 0 0 0 0)))
        (check-equal?
         (begin (vector-reverse-copy! target 1 source 1)
                target)
         '#(0 5 4 3 2)))
      (let ((source (vector 1 2 3 4 5))
            (target (vector 0 0 0 0 0)))
        (check-equal?
         (begin (vector-reverse-copy! target 1 source 1 4)
                target)
         '#(0 4 3 2 0))))
     ))
  )
