#!r6rs

(library (tests r6rs arithmetic bitwise)
  (export run-arithmetic-bitwise-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-arithmetic-bitwise-tests)

    (test (bitwise-first-bit-set 0)         -1)
    (test (bitwise-first-bit-set 1)         0)
    (test (bitwise-first-bit-set -4)        2)
    
    (test (bitwise-arithmetic-shift -6 -1) -3)
    (test (bitwise-arithmetic-shift -5 -1) -3)
    (test (bitwise-arithmetic-shift -4 -1) -2)
    (test (bitwise-arithmetic-shift -3 -1) -2)
    (test (bitwise-arithmetic-shift -2 -1) -1)
    (test (bitwise-arithmetic-shift -1 -1) -1)
    
    (test (bitwise-reverse-bit-field #b1010010 1 4)    88) ; #b1011000
          
    ;;
    ))

