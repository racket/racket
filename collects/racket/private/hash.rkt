(module hash "pre-base.rkt"
  (define (hash-domain table)
    (hash-map table (λ (k v) k)))
  
  (define (hash-range table)
    (hash-map table (λ (k v) v)))
  
  (define (hash->list table)
    (hash-map table cons))
  
  (provide hash-domain
           hash-range
           hash->list))