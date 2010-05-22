#lang scheme

(require "checks.ss"
         "../hash.ss")

(provide hash-suite)

(define hash-suite
  (test-suite "hash.ss"
    (test-suite "hash"
      (test (check-equal? (hash [1 'a] [2 'b])
                          #hash([1 . a] [2 . b])))
      (test (check-equal? (hash #:eq [1 'a] [2 'b])
                          #hasheq([1 . a] [2 . b])))
      (test (check-equal? (hash #:eqv [1 'a] [2 'b])
                          #hasheqv([1 . a] [2 . b])))
      (test (check-equal? (hash #:equal [1 'a] [2 'b])
                          #hash([1 . a] [2 . b]))))
    (test-suite "hash!"
      (test (check-equal? (hash! [1 'a] [2 'b])
                          (hash-copy #hash([1 . a] [2 . b]))))
      (test (check-equal? (hash! #:eq [1 'a] [2 'b])
                          (hash-copy #hasheq([1 . a] [2 . b]))))
      (test (check-equal? (hash! #:eqv #:weak [1 'a] [2 'b])
                          (make-weak-hasheqv '([1 . a] [2 . b]))))
      (test (check-equal? (hash! #:weak #:equal [1 'a] [2 'b])
                          (make-weak-hash '([1 . a] [2 . b])))))
    (test-suite "hash-equal?"
      (test (check-true (hash-equal? #hash())))
      (test (check-false (hash-equal? #hasheq())))
      (test (check-false (hash-equal? #hasheqv()))))
    (test-suite "hash-ref/check"
      (test-ok (check-equal? (hash-ref/check #hash([1 . one] [2 . two]) 1)
                             'one))
      (test-bad (hash-ref/check #hash([1 . one] [2 . two]) 3)))
    (test-suite "hash-ref/identity"
      (test-ok (check-equal? (hash-ref/identity #hash([1 . one] [2 . two]) 1)
                             'one))
      (test-ok (check-equal? (hash-ref/identity #hash([1 . one] [2 . two]) 3)
                             3)))
    (test-suite "hash-ref/default"
      (test-ok (check-equal? (hash-ref/default #hash([1 . one] [2 . two]) 1 '?)
                             'one))
      (test-ok (check-equal? (hash-ref/default #hash([1 . one] [2 . two]) 3 '?)
                             '?)))
    (test-suite "hash-ref/failure"
      (test-ok (define x 7)
               (define (f) (set! x (+ x 1)) x)
               (check-equal? (hash-ref/failure #hash([1 . one] [2 . two]) 1 f)
                             'one)
               (check-equal? x 7)
               (check-equal? (hash-ref/failure #hash([1 . one] [2 . two]) 3 f)
                             8)
               (check-equal? x 8)))
    (test-suite "hash-has-key?"
      (test-ok (check-equal? (hash-has-key? #hash([1 . one] [2 . two]) 1) #t))
      (test-ok (check-equal? (hash-has-key? #hash([1 . one] [2 . two]) 3) #f)))
    (test-suite "hash-domain"
      (test-ok (check-equal? (hash-domain #hash([1 . one] [2 . two])) '(1 2))))
    (test-suite "hash-range"
      (test-ok (check-equal? (hash-range #hash([1 . one] [2 . two]))
                             '(one two))))
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

