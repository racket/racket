#lang typed/scheme

;; Inference fails when #:when clauses are used, except when there's
;; only one, and it's the last clause.
;; for:, for*: are unaffected, since they currently expand their
;; #:when clauses manually, unlike for/list: and co, who punt it to
;; non-annotated versions of the macros, to avoid breaking semantics.
;; for/fold:, for*/fold:, for/lists: and for*/lists: are not affected
;; either. The inferencer can handle their expansion, apparently no
;; matter how many #:when clauses we throw at them.
;; Of course, for*/list: and co won't work, since they are equivalent
;; to for/list: and co with #:when clauses.
;; These are currently documented as not working.
(for/list: : (Listof Integer)
           ((i : Exact-Positive-Integer '(1 2 3))
            #:when (odd? i)
            (j : Exact-Positive-Integer '(10 20 30)))
           (+ i j 10))

(for/list: : (Listof Integer)
           (#:when #t
            (i : Exact-Positive-Integer '(1 2 3))
            (j : Exact-Positive-Integer '(10 20 30)))
           (+ i j 10))

(for*/list: : (Listof (Listof Integer))
            ((i : Exact-Positive-Integer '(1 2 3))
             (j : Exact-Positive-Integer '(10 20 30)))
            (list i j))

;; The right type for the return value would be:
;; (values (Listof Integer) (Listof Integer)).
;; The problem here is with the error message. Somehow, source location
;; information is lost and the whole module is blamed.
(for/lists: : (Listof Integer)
            ((x : (Listof Integer))
             (y : (Listof Integer)))
            ((i : Exact-Positive-Integer '(1 2 3))
             (j : Exact-Positive-Integer '(10 20 30)))
            (values i j))

;; This is a legitimate use of multi-valued seq-exprs, but it causes
;; the typechecker to throw an internal error.
;; Multi-valued seq-exprs are currently turned off and documented as
;; not working.
(for/list: : (Listof Integer)
           ((([i : Exact-Positive-Integer]
              [j : Exact-Positive-Integer])
             (list (values 1 10) (values 2 20) (values 2 30)))
            #:when (odd? i))
           (+ i j 10))

;; Types can't be inferred for the expansion of for/first: at all.
(for/first: : Integer
            ((i : Exact-Positive-Integer '(1 2 3)))
            i)

;; Same for for/last:
(for/last: : (Option Integer)
           ((i : Exact-Positive-Integer '(1 2 3)))
           i)

;; unlike the usual cases with #:when clauses, inference does something, but does it wrong
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

;; for/vector: would need stronger inference. same for for*/vector and
;; both flvector variants
(for/vector: : (Vectorof Integer)
             ((x : Integer (in-range 10)))
             x)
