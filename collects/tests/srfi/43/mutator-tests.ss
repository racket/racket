(module mutator-tests mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
  (require (all-except (lib "mutators.ss" "srfi" "43") vector-fill!)
           (rename (lib "mutators.ss" "srfi" "43") s:vector-fill! vector-fill!))
  
  (provide mutator-tests)
  
  (define mutator-tests
    (make-test-suite
     "All tests for mutator"
     (make-test-case
      "vector-swap!"
      (let ((vec '#(a b c d e)))
        (assert-equal?
         (begin (vector-swap! vec 1 3)
                vec)
         '#(a d c b e)))
      (let ((vec '#(0 1 2)))
        (assert-equal?
         (begin (vector-swap! vec 1 1)
                vec)
         '#(0 1 2))))
     
     (make-test-case
      "vector-fill!"
      (let ((vec '#(1 2 3 4 5)))
        (assert-equal?
         (begin (s:vector-fill! vec 0)
                vec)
         '#(0 0 0 0 0)))
      (let ((vec '#(1 2 3 4 5)))
        (assert-equal?
         (begin (s:vector-fill! vec 0 1)
                vec)
         '#(1 0 0 0 0)))
      (let ((vec '#(1 2 3 4 5)))
        (assert-equal?
         (begin (s:vector-fill! vec 0 1 4)
                vec)
         '#(1 0 0 0 5))))
     
     (make-test-case
      "vector-reverse!"
      (let ((vec '#(1 2 3 4 5)))
        (assert-equal?
         (begin (vector-reverse! vec)
                vec)
         '#(5 4 3 2 1)))
      (let ((vec '#(1 2 3 4 5)))
        (assert-equal?
         (begin (vector-reverse! vec 1)
                vec)
         '#(1 5 4 3 2)))
      (let ((vec '#(1 2 3 4 5)))
        (assert-equal?
         (begin (vector-reverse! vec 1 4)
                vec)
         '#(1 4 3 2 5))))
     
     (make-test-case
      "vector-copy!"
      (let ((source '#(1 2 3 4 5))
            (target '#(0 0 0 0 0)))
        (assert-equal?
         (begin (vector-copy! target 0 source)
                target)
         source))
      (let ((source '#(1 2 3 4 5))
            (target '#(0 0 0 0 0)))
        (assert-equal?
         (begin (vector-copy! target 1 source 1)
                target)
         '#(0 2 3 4 5)))
      (let ((source '#(1 2 3 4 5))
            (target '#(0 0 0 0 0)))
        (assert-equal?
         (begin (vector-copy! target 1 source 1 4)
                target)
         '#(0 2 3 4 0))))
     
     (make-test-case
      "vector-reverse-copy!"
      (let ((source '#(1 2 3 4 5))
            (target '#(0 0 0 0 0)))
        (assert-equal?
         (begin (vector-reverse-copy! target 0 source)
                target)
         '#(5 4 3 2 1)))
      (let ((source '#(1 2 3 4 5))
            (target '#(0 0 0 0 0)))
        (assert-equal?
         (begin (vector-reverse-copy! target 1 source 1)
                target)
         '#(0 5 4 3 2)))
      (let ((source '#(1 2 3 4 5))
            (target '#(0 0 0 0 0)))
        (assert-equal?
         (begin (vector-reverse-copy! target 1 source 1 4)
                target)
         '#(0 4 3 2 0))))
     ))
  )
