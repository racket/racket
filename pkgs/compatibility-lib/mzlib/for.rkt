(module for racket/base
  (provide for/fold for*/fold
           for for*
           for/list for*/list
           for/lists for*/lists
           for/and for*/and
           for/or for*/or
           for/first for*/first
           for/last for*/last

           for/fold/derived for*/fold/derived
           
           in-range
           in-naturals
           in-list
           in-vector
           in-string
           in-bytes
           in-input-port-bytes
           in-input-port-chars
           (rename-out [in-hash in-hash-table]
                       [in-hash-keys in-hash-table-keys]
                       [in-hash-values in-hash-table-values]
                       [in-hash-pairs in-hash-table-pairs])
           
           in-parallel
           stop-before
           stop-after
           in-indexed
           
           sequence?
           sequence-generate
           
           define-sequence-syntax
           make-do-sequence
           :do-in))
