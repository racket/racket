#lang typed/scheme

(: table (HashTable Integer (-> Integer)))
(define table
  (make-immutable-hash null))

(: lookup (Integer -> Integer))
(define (lookup n)

  (: thunk (-> Integer))
  (define thunk
    (hash-ref table n (lambda () 0)))

  (thunk))

(lookup 1)
