(module hash "pre-base.rkt"
  (define (hash-domain table)
    (for/list ([i (in-hash-keys table)]) i))
  
  (define (hash-range table)
    (for/list ([i (in-hash-values table)]) i))
  
  (provide hash-domain
           hash-range)
  )