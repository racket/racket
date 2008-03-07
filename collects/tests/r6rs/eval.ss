#!r6rs

(library (tests r6rs eval)
  (export run-eval-tests)
  (import (rnrs)
          (rnrs eval)
          (tests r6rs test))

  (define (run-eval-tests)

    (test (eval '(let ((x 3)) x)
                (environment '(rnrs)))
          3)

    (test (eval
           '(eval:car (eval:cons 2 4))
           (environment
            '(prefix (only (rnrs) car cdr cons null?)
                     eval:)))
          2)

    ;;
    ))

