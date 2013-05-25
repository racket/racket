#!r6rs

(library (tests r6rs sorting)
  (export run-sorting-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-sorting-tests)

    (test (list-sort < '(3 5 2 1)) '(1 2 3 5))
    (test (vector-sort < '#(3 5 2 1)) '#(1 2 3 5))

    (let ([v (vector 3 5 2 1)])
      (test/unspec (vector-sort! < v))
      (test v '#(1 2 3 5)))

    ;;
    ))

