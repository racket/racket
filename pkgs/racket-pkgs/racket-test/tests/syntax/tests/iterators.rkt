#lang racket
(require syntax/id-table)
(let*-values ([(a b c) (values #'a #'b #'c)]
              [(res) (hash a 0 b 1 c 2)]
              [(t) (make-free-id-table res)])
 (unless
     (equal? res
             (for/hash ([(k v) (in-free-id-table t)])
               (values k v)))
   (error 'free-id-table-test "Unexpected failure for in-free-id-table"))
 (unless
     (equal? (set a b c)
             (for/set ([k (in-free-id-table-keys t)]) k))
   (error 'free-id-table-test "Unexpected failure for in-free-id-table-keys"))
 (unless
     (equal? (set 0 1 2)
             (for/set ([v (in-free-id-table-values t)]) v))
   (error 'free-id-table-test "Unexpected failure for in-free-id-table-values")))