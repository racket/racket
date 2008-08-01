#!r6rs

(import (rnrs) (rnrs eval) (tests r6rs test))

(define-syntax test-library
   (syntax-rules ()
     [(_ test-proc library-name)
      (test/unspec (eval '(test-proc) (environment 'library-name)))]))

(test-library run-base-tests               (tests r6rs base))
(test-library run-reader-tests             (tests r6rs reader))
(test-library run-unicode-tests            (tests r6rs unicode))
(test-library run-bytevectors-tests        (tests r6rs bytevectors))
(test-library run-lists-tests              (tests r6rs lists))
(test-library run-sorting-tests            (tests r6rs sorting))
(test-library run-control-tests            (tests r6rs control))
(test-library run-records-syntactic-tests  (tests r6rs records syntactic))
(test-library run-records-procedural-tests (tests r6rs records procedural))
(test-library run-exceptions-tests         (tests r6rs exceptions))
(test-library run-conditions-tests         (tests r6rs conditions))
(test-library run-io-ports-tests           (tests r6rs io ports))
(test-library run-io-simple-tests          (tests r6rs io simple))
(test-library run-programs-tests           (tests r6rs programs))
(test-library run-arithmetic-fixnums-tests (tests r6rs arithmetic fixnums))
(test-library run-arithmetic-flonums-tests (tests r6rs arithmetic flonums))
(test-library run-arithmetic-bitwise-tests (tests r6rs arithmetic bitwise))
(test-library run-syntax-case-tests        (tests r6rs syntax-case))
(test-library run-hashtables-tests         (tests r6rs hashtables))
(test-library run-enums-tests              (tests r6rs enums))
(test-library run-eval-tests               (tests r6rs eval))
(test-library run-mutable-pairs-tests      (tests r6rs mutable-pairs))
(test-library run-mutable-strings-tests    (tests r6rs mutable-strings))
(test-library run-r5rs-tests               (tests r6rs r5rs))
(test-library run-contrib-tests            (tests r6rs contrib))

(report-test-results)

