#lang racket

(require rackunit rackunit/text-ui unstable/dict "helpers.rkt")

(define (dict=? a b)
  (and (subdict? a b)
       (subdict? b a)))

(define (subdict? a b)
  (for/and ([(k v) (in-dict a)])
    (and (dict-has-key? b k)
         (equal? (dict-ref b k) v))))

(define (check/dict a b) (check dict=? a b))

(run-tests
 (test-suite "dict.rkt"
   (test-suite "Accessors"
     (test-suite "dict-empty?"
       (test (check-true (dict-empty? '())))
       (test (check-false (dict-empty? '([1 . a] [2 . b]))))))
   (test-suite "Combination"
     (test-suite "dict-union"
       (test-ok (dict-union '([1 . one] [2 . two]) '([3 . three] [4 . four]))
                '([4 . four] [3 . three] [1 . one] [2 . two])))
     (test-suite "dict-union!"
       (test-ok (define d (make-hash))
                (dict-union! d '([1 . one] [2 . two]))
                (dict-union! d '([3 . three] [4 . four]))
                (check-equal?
                 (hash-copy #hash([1 . one] [2 . two] [3 . three] [4 . four]))
                 d))))))
