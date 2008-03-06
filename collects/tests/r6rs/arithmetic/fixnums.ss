#!r6rs

(library (tests r6rs arithmetic fixnums)
  (export run-arithmetic-fixnums-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-arithmetic-fixnums-tests)

    (test/exn (fx- (least-fixnum)) &implementation-restriction)

    (test (fxfirst-bit-set 0)         -1)
    (test (fxfirst-bit-set 1)         0)
    (test (fxfirst-bit-set -4)        2)

    (test (fxreverse-bit-field #b1010010 1 4)     88) ; #b1011000

    ;;
    ))

