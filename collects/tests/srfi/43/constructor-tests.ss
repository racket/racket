(module constructor-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
  (require (lib "constructors.ss" "srfi" "43"))

  (provide constructor-tests)

  (define constructor-tests
    (make-test-suite
     "All tests for constructor"
     (make-test-case
      "vector-unfold"
      (assert-equal?
       (vector-unfold values 10)
       #(0 1 2 3 4 5 6 7 8 9)
       "No seed")
      (assert-equal?
       (vector-unfold (lambda (i x) (values x (- x 1))) 10 0)
       #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
       "Single seed")
      (assert-equal?
       (vector-unfold (lambda (i a b) (values (/ a b) a b)) 4 5 5)
       #(1 1 1 1)
       "Two seeds"))

     (make-test-case
      "vector-unfold-right"
      (assert-equal?
       (vector-unfold-right values 10)
       #(0 1 2 3 4 5 6 7 8 9)
       "No seed")
      (assert-equal?
       (vector-unfold-right
        (lambda (i x) (values x (- x 1)))
        10
        0)
       #(-9 -8 -7 -6 -5 -4 -3 -2 -1 0)
       "Single seed")
      (assert-equal?
       (vector-unfold-right
        (lambda (i a b) (values (/ a b) a b))
        4
        5
        5)
       #(1 1 1 1)
       "Two seeds"))

     (make-test-case
      "vector-copy"
      (assert-equal?
       (vector-copy '#(a b c d e f g h i))
       #(a b c d e f g h i))
      (assert-equal?
       (vector-copy '#(a b c d e f g h i) 6)
       #(g h i))
      (assert-equal?
       (vector-copy '#(a b c d e f g h i) 3 6)
       #(d e f))
      (assert-equal?
       (vector-copy '#(a b c d e f g h i) 6 12 'x)
       #(g h i x x x)))

     (make-test-case
      "vector-reverse-copy"
      (assert-equal?
       (vector-reverse-copy '#(5 4 3 2 1 0) 1 5)
       #(1 2 3 4))
      (assert-equal?
       (vector-reverse-copy '#(5 4 3 2 1 0) 2)
       #(0 1 2 3))
      (assert-equal?
       (vector-reverse-copy '#(5 4 3 2 1 0))
       #(0 1 2 3 4 5)))

     (make-test-case
      "vector-append"
      (assert-equal?
       (vector-append '#(x) '#(y))
       #(x y))
      (assert-equal?
       (vector-append '#(a) '#(b c d))
       #(a b c d))
      (assert-equal?
       (vector-append '#(a #(b)) '#(#(c)))
       #(a #(b) #(c)))
      (assert-equal?
       (vector-append '#(1 2) '#(3) '#(4 5 6 7))
       #(1 2 3 4 5 6 7))
      (assert-equal?
       (vector-append)
       #()))

     (make-test-case
      "vector-concatenate"
      (assert-equal?
       (vector-concatenate '(#(x) #(y)))
       #(x y))
      (assert-equal?
       (vector-concatenate '(#(a) #(b c d)))
       #(a b c d))
      (assert-equal?
       (vector-concatenate '(#(a #(b)) #(#(c))))
       #(a #(b) #(c)))
      (assert-equal?
       (vector-concatenate '(#(1 2) #(3) #(4 5 6 7)))
       #(1 2 3 4 5 6 7))
      (assert-equal?
       (vector-concatenate '())
       #()))
     ))
  )
