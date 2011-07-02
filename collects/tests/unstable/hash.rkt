#lang racket

(require rackunit rackunit/text-ui unstable/hash "helpers.rkt")

(run-tests
 (test-suite "hash.rkt"
   (test-suite "hash-union"
     (test-ok (hash-union #hash([1 . one] [2 . two])
                          #hash([3 . three] [4 . four]))
              #hash([4 . four] [3 . three] [1 . one] [2 . two])))
   (test-suite "hash-union!"
     (test-ok (define h (make-hash))
              (hash-union! h #hash([1 . one] [2 . two]))
              (hash-union! h #hash([3 . three] [4 . four]))
              (check-equal? (hash-copy
                             #hash([1 . one] [2 . two] [3 . three] [4 . four]))
                            h)))))
