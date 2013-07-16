(module iteration-tests mzscheme
  
  (require rackunit)
  (require srfi/43/vector-lib)
  
  (provide iteration-tests)
  
  (define iteration-tests
    (test-suite
     "All tests for iteration"
     (test-case
      "vector-fold"
      (check =
              (vector-fold (lambda (index len str) (max (string-length str) len))
                           0 '#("abcde" "1234" "bar"))
              5)
      (check-equal?
       (vector-fold (lambda (index tail elt) (cons elt tail))
                    '() '#(1 2 3 4 5))
       '(5 4 3 2 1)
       "reverse-vector->list")
      (check =
              (vector-fold (lambda (index counter n)
                             (if (even? n) (+ counter 1) counter))
                           0 '#(1 2 3 4 5))
              2)
      (check-equal?
       (vector-fold (lambda (index tail elt1 elt2)
                      (cons (list elt1 elt2) tail))
                    '()
                    '#(1 2 3 4 5)
                    '#(a b c d))
       '((4 d) (3 c) (2 b) (1 a))
       "two vectors"))
     
     (test-case
      "vector-fold-right"
      (check-equal?
       (vector-fold-right (lambda (index tail elt) (cons elt tail))
                          '() '#(a b c d))
       '(a b c d)
       "vector->list")
      (check-equal?
       (vector-fold-right (lambda (index tail elt1 elt2)
                            (cons (list elt1 elt2) tail))
                          '()
                          '#(1 2 3 4 5)
                          '#(a b c d))
       '((1 a) (2 b) (3 c) (4 d))
       "two vectors"))
     
     (test-case
      "vector-map"
      (check-equal?
       (vector-map (lambda (i x) (* x x))
                   '#(1 2 3 4))
       '#(1 4 9 16))
      (check-equal?
       (vector-map (lambda (i x y) (* x y))
                   (vector-unfold (lambda (i x) (values x (+ x 1))) 5 1)
                   (vector-unfold (lambda (i x) (values x (- x 1))) 5 5))
       '#(5 8 9 8 5))
      (check-equal?
       (vector-map (lambda (i elt) (+ i elt)) '#(1 2 3 4))
       '#(1 3 5 7)))
     
     (test-case
      "vector-map!"
      (let ((vec (vector 1 2 3 4)))
        (check-equal?
         (begin (vector-map! (lambda (i x) (* x x))
                             vec)
                vec)
         '#(1 4 9 16)))
      (let ((vec1 (vector-unfold (lambda (i x) (values x (+ x 1))) 5 1))
            (vec2 (vector-unfold (lambda (i x) (values x (- x 1))) 5 5)))
        (check-equal?
         (begin (vector-map! (lambda (i x y) (* x y))
                             vec1 vec2)
                vec1)
         '#(5 8 9 8 5)))
      (let ((vec (vector 1 2 3 4)))
        (check-equal?
         (begin (vector-map! (lambda (i elt) (+ i elt))
                             vec)
                vec)
         '#(1 3 5 7))))
     
     (test-case
      "vector-for-each"
      (let ((vec1 (vector 1 2 3 4))
            (vec2 (make-vector 4)))
        (check-equal?
         (begin (vector-for-each (lambda (i elt)
                                   (vector-set! vec2 i (+ i elt)))
                                 vec1)
                vec2)
         '#(1 3 5 7)))
      (let ((vec1 (vector-unfold (lambda (i x) (values x (+ x 1))) 5 1))
            (vec2 (vector-unfold (lambda (i x) (values x (- x 1))) 5 5))
            (vec3 (make-vector 5)))
        (check-equal?
         (begin (vector-for-each (lambda (i x y)
                                   (vector-set! vec3 i (* x y)))
                                 vec1 vec2)
                vec3)
         '#(5 8 9 8 5))))
     
     (test-case
      "vector-count"
      (check =
              (vector-count (lambda (i elt) (even? elt)) '#(3 1 4 1 5 9 2 5 6))
              3)
      (check =
              (vector-count (lambda (i x y) (< x y)) '#(1 3 6 9) '#(2 4 6 8 10 12))
              2))
     ))
  )
