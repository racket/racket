#!r6rs

(library (tests r6rs programs)
  (export run-programs-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-programs-tests)

    (test (list? (command-line)) #t)
    (test (string? (car (command-line))) #t)
      
    ;;
    ))

