#!r6rs

(library (tests r6rs control)
  (export run-control-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-control-tests)

    (test (when (> 3 2) 'greater) 'greater)
    (test/unspec (when (< 3 2) 'greater))
    (test/unspec (unless (> 3 2) 'less))
    (test (unless (< 3 2) 'less) 'less)

    (test (do ((vec (make-vector 5))
               (i 0 (+ i 1)))
              ((= i 5) vec)
            (vector-set! vec i i))
          '#(0 1 2 3 4))

    (test (let ((x '(1 3 5 7 9)))
            (do ((x x (cdr x))
                 (sum 0 (+ sum (car x))))
                ((null? x) sum)))
          25)

    (let ([foo
           (case-lambda 
            (() 'zero)
            ((x) (list 'one x))
            ((x y) (list 'two x y))
            ((a b c d . e) (list 'four a b c d e))
            (rest (list 'rest rest)))])

      (test (foo) 'zero)
      (test (foo 1) '(one 1))
      (test (foo 1 2) '(two 1 2))
      (test (foo 1 2 3) '(rest (1 2 3)))
      (test (foo 1 2 3 4) '(four 1 2 3 4 ())))
      
    ;;
    ))

