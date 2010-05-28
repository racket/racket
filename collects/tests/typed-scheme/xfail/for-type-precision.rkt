#lang typed/scheme

;; I would have to be annotated with its most precise type (Exact-Positive-Integer) for this to work
(for: : Void ((i : Integer '(1 2 3)))
      (display i))

;; same here, would need type (U (List 'a 't) (List 'c 'g))
(for: : Void
      ([from-to : (List Symbol Symbol)
                '([a t]
                  [c g])])
      #t)

;; wants (U False True), which should be the same as Boolean, but apparently isn't
(for: : Void
      ((k : Boolean #(#t #f))
       #:when k)
      (display k))

;; unlike the usual cases with #:when clauses (see for-inference.rkt), inference does something, but does it wrong
(for/list: : (Listof Integer)
           (#:when #t
            (i : Exact-Positive-Integer '(1 2 3))
            (j : Exact-Positive-Integer '(10 20 30)))
           (+ i j 10))

;; that same bug makes for/hash:, for/hasheq: and for/hasheqv: unusable
;; this infers Nothing for the type of the elements of the HashTable
;; since they don't work, these functions are not currently documented
(for/hash: : (HashTable Integer Char)
           ((i : Exact-Positive-Integer '(1 2 3))
            (j : Char "abc"))
           (values i j))
;; same thing for for/and:
(for/and: : Boolean
          ((i : Exact-Positive-Integer '(1 2 3)))
          (< i 3))
