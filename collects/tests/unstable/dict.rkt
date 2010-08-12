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
 (test-suite "dict.ss"
   (test-suite "Constructors"
     (test-suite "empty-dict"
       (test (check/dict (empty-dict) '()))
       (test (check/dict (empty-dict #:mutable? #t) '()))
       (test (check/dict (empty-dict #:weak? #t) '()))
       (test (check/dict (empty-dict #:compare 'eqv) '())))
     (test-suite "make-dict"
       (test (check/dict (make-dict '([1 . a] [2 . b])) '([1 . a] [2 . b])))
       (test (check/dict (make-dict '([1 . a] [2 . b]) #:mutable? #t)
                         '([1 . a] [2 . b])))
       (test (check/dict (make-dict '([1 . a] [2 . b]) #:weak? #t)
                         '([1 . a] [2 . b])))
       (test (check/dict (make-dict '([1 . a] [2 . b]) #:compare 'eqv)
                         '([1 . a] [2 . b]))))
     (test-suite "custom-dict"
       (test (let* ([table (custom-dict = add1 sub1 #:mutable? #t)])
               (dict-set! table 1 'a)
               (dict-set! table 2 'b)
               (check/dict table '([1 . a] [2 . b]))))))
   (test-suite "Lookup"
     (test-suite "dict-ref/check"
       (test-ok (check-equal? (dict-ref/check '([1 . one] [2 . two]) 1) 'one))
       (test-bad (dict-ref/check '([1 . one] [2 . two]) 3)))
     (test-suite "dict-ref/identity"
       (test-ok (check-equal? (dict-ref/identity '([1 . one] [2 . two]) 1)
                              'one))
       (test-ok (check-equal? (dict-ref/identity '([1 . one] [2 . two]) 3) 3)))
     (test-suite "dict-ref/default"
       (test-ok (check-equal? (dict-ref/default '([1 . one] [2 . two]) 1 '?)
                              'one))
       (test-ok (check-equal? (dict-ref/default '([1 . one] [2 . two]) 3 '?)
                              '?)))
     (test-suite "dict-ref/failure"
       (test-ok (define x 7)
                (define (f) (set! x (+ x 1)) x)
                (check-equal? (dict-ref/failure '([1 . one] [2 . two]) 1 f)
                              'one)
                (check-equal? x 7)
                (check-equal? (dict-ref/failure '([1 . one] [2 . two]) 3 f) 8)
                (check-equal? x 8))))
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
                 d))))
   (test-suite "Property"
     (test-suite "wrapped-dict-property"
       (test
        (let ()
          (define (unwrap-table d) (table-dict d))
          (define (wrap-table d) (make-table d))
          (define (wrapped? d) (table? d))
          (define-struct table [dict]
            #:transparent
            #:property prop:dict
            (wrapped-dict-property
             #:unwrap unwrap-table
             #:wrap wrap-table
             #:predicate wrapped?))
          (check-true (dict? (make-table '([1 . a] [2 . b]))))
          (check/dict (make-table '([1 . a] [2 . b])) '([1 . a] [2 . b]))
          (check-equal? (dict-ref (make-table '([1 . a] [2 . b])) 1) 'a)
          (let* ([s (dict-set (make-table '([1 . a] [2 . b])) 3 'c)])
            (check-true (table? s))
            (check/dict s '([1 . a] [2 . b] [3 . c])))))))))
