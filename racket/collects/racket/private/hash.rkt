(module hash "pre-base.rkt"
  (define (hash-keys h)
    (let loop ([pos (hash-iterate-first h)])
      (if pos
          (cons (hash-iterate-key h pos)
                (loop (hash-iterate-next h pos)))
          null)))
  
  (define (hash-values table)
    (hash-map table (λ (k v) v)))
  
  (define (hash->list table)
    (hash-map table cons))
  
  (define (hash-set* table . pairs)
    (unless (even? (length pairs))
      (error 'hash-set* "expected an even number of association elements, but received an odd number: ~e" pairs))
    (let loop ([table table]
               [pairs pairs])
      (if (null? pairs)
          table
          (loop (hash-set table (car pairs) (cadr pairs))
                (cddr pairs)))))
  
  (define (hash-set*! table . pairs)
    (unless (even? (length pairs))
      (error 'hash-set*! "expected an even number of association elements, but received an odd number: ~e" pairs))
    (let loop ([pairs pairs])
      (unless (null? pairs)
        (hash-set! table (car pairs) (cadr pairs))
        (loop (cddr pairs)))))
  
  (provide hash-keys
           hash-values
           hash->list
           hash-set*
           hash-set*!))
