(module constructor-tests mzscheme

  (require rackunit)
  (require srfi/43/vector-lib)

  (provide constructor-tests)

  (define constructor-tests
    (test-suite
     "All tests for constructor"
     (test-case
      "vector-unfold"
      (check-equal?
       (vector-unfold values 10)
       #(0 1 2 3 4 5 6 7 8 9)
       "No seed")
      (check-equal?
       (vector-unfold (lambda (i x) (values x (- x 1))) 10 0)
       #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
       "Single seed")
      (check-equal?
       (vector-unfold (lambda (i a b) (values (/ a b) a b)) 4 5 5)
       #(1 1 1 1)
       "Two seeds"))

     (test-case
      "vector-unfold-right"
      (check-equal?
       (vector-unfold-right values 10)
       #(0 1 2 3 4 5 6 7 8 9)
       "No seed")
      (check-equal?
       (vector-unfold-right
        (lambda (i x) (values x (- x 1)))
        10
        0)
       #(-9 -8 -7 -6 -5 -4 -3 -2 -1 0)
       "Single seed")
      (check-equal?
       (vector-unfold-right
        (lambda (i a b) (values (/ a b) a b))
        4
        5
        5)
       #(1 1 1 1)
       "Two seeds"))

     (test-case
      "vector-copy"
      (check-equal?
       (vector-copy '#(a b c d e f g h i))
       #(a b c d e f g h i))
      (check-equal?
       (vector-copy '#(a b c d e f g h i) 6)
       #(g h i))
      (check-equal?
       (vector-copy '#(a b c d e f g h i) 3 6)
       #(d e f))
      (check-equal?
       (vector-copy '#(a b c d e f g h i) 6 12 'x)
       #(g h i x x x)))

     (test-case
      "vector-reverse-copy"
      (check-equal?
       (vector-reverse-copy '#(5 4 3 2 1 0) 1 5)
       #(1 2 3 4))
      (check-equal?
       (vector-reverse-copy '#(5 4 3 2 1 0) 2)
       #(0 1 2 3))
      (check-equal?
       (vector-reverse-copy '#(5 4 3 2 1 0))
       #(0 1 2 3 4 5)))

     (test-case
      "vector-append"
      (check-equal?
       (vector-append '#(x) '#(y))
       #(x y))
      (check-equal?
       (vector-append '#(a) '#(b c d))
       #(a b c d))
      (check-equal?
       (vector-append '#(a #(b)) '#(#(c)))
       #(a #(b) #(c)))
      (check-equal?
       (vector-append '#(1 2) '#(3) '#(4 5 6 7))
       #(1 2 3 4 5 6 7))
      (check-equal?
       (vector-append)
       #()))

     (test-case
      "vector-concatenate"
      (check-equal?
       (vector-concatenate '(#(x) #(y)))
       #(x y))
      (check-equal?
       (vector-concatenate '(#(a) #(b c d)))
       #(a b c d))
      (check-equal?
       (vector-concatenate '(#(a #(b)) #(#(c))))
       #(a #(b) #(c)))
      (check-equal?
       (vector-concatenate '(#(1 2) #(3) #(4 5 6 7)))
       #(1 2 3 4 5 6 7))
      (check-equal?
       (vector-concatenate '())
       #()))
     ))
  )
