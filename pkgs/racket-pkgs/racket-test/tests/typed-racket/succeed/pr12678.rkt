#lang typed/racket

(define: memo : (HashTable Natural String)  (make-immutable-hash empty))
(define strs '("Hello" "Goodbye"))

(for/fold: : (HashTable Natural String)
    ([memo : (HashTable Natural String)  (make-immutable-hash empty)])
    ([i : Natural  (in-naturals)] [str : String  (in-list strs)])
  (hash-set memo i str))
