#lang typed/racket/base

(require typed/rackunit)

(check-pred
 hash?
 (for/hash: : (HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash?
 (for/hash: : (HashTable Integer Symbol)
            ((a (list 1 2 3))
             (b '(a b c)))
  (values a b)))

(check-pred
 hash?
 (for*/hasheq: : (HashTable Integer Symbol)
             ((a (list 1 2 3))
              (b '(a b c)))
  (values a b)))


(check-pred
 hash-eq?
 (for/hasheq: : (HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash-eq?
 (for/hasheq: : (HashTable Integer Symbol)
            ((a (list 1 2 3))
             (b '(a b c)))
  (values a b)))

(check-pred
 hash-eq?
 (for*/hasheq: : (HashTable Integer Symbol)
             ((a (list 1 2 3))
              (b '(a b c)))
  (values a b)))


(check-pred
 hash-eqv?
 (for/hasheqv: : (HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash-eqv?
 (for/hasheqv: : (HashTable Integer Symbol)
            ((a (list 1 2 3))
             (b '(a b c)))
  (values a b)))

(check-pred
 hash-eqv?
 (for*/hasheqv: : (HashTable Integer Symbol)
             ((a (list 1 2 3))
              (b '(a b c)))
  (values a b)))



(for*/hash: : (HashTable Number Number)
            ((v : Number '(1 2 3))
             (x : Number '(4 5 6)))
     (values v x))



(for/hash: : (HashTable Symbol Symbol)
    ((v : Symbol '(a b c)))
  (values v v))

(for/hash: : (HashTable Symbol Symbol)
    ([(k b) (in-hash (make-immutable-hash '((a . a) (b . b))))])
  (values k b))
