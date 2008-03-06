#!r6rs

(library (tests r6rs io ports)
  (export run-io-ports-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-io-ports-tests)

    (test (eqv? (eof-object) (eof-object)) #t)
    (test (eq? (eof-object) (eof-object)) #t)

    ;;
    ))

