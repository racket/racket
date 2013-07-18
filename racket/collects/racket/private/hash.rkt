(module hash "pre-base.rkt"
  (define (hash-keys h)
    (let loop ([pos (hash-iterate-first h)])
      (if pos
          (cons (hash-iterate-key h pos)
                (loop (hash-iterate-next h pos)))
          null)))

  (define (hash-values table)
    (unless (hash? table)
      (raise-argument-error 'hash-values "hash?" table))
    (hash-map table (λ (k v) v)))

  (define (hash->list table)
    (unless (hash? table)
      (raise-argument-error 'hash->list "hash?" table))
    (hash-map table cons))

  (define (hash-set* table . pairs0)
    (unless (and (hash? table) (immutable? table))
      (raise-argument-error 'hash-set*
                            "(and/c hash? immutable?)"
                            table))
    (let loop ([table table]
               [pairs pairs0])
      (cond
        [(null? pairs) table]
        [(null? (cdr pairs))
         (raise-arguments-error
          'hash-set*
          "expected an even number of association elements, but received an odd number"
          "association elements"
          pairs0)]
        [else (loop (hash-set table (car pairs) (cadr pairs))
                    (cddr pairs))])))

  (define (hash-set*! table . pairs0)
    (unless (and (hash? table) (not (immutable? table)))
      (raise-argument-error 'hash-set*!
                            "(and/c hash? (not/c immutable?))"
                            table))
    (let loop ([pairs pairs0])
      (cond
        [(null? pairs) (void)]
        [(null? (cdr pairs))
         (raise-arguments-error
          'hash-set*!
          "expected an even number of association elements, but received an odd number"
          "association elements"
          pairs0)]
        [else
         (hash-set! table (car pairs) (cadr pairs))
         (loop (cddr pairs))])))

  ;; This could probably be implemented in O(1) internally by simply
  ;; throwing away the hash table's array and allocating a new one.
  ;; At the Racket level, we'll have to make do with O(n) iteration.
  (define (hash-clear! table)
    (unless (and (hash? table) (not (immutable? table)))
      (raise-argument-error 'hash-clear!
                            "(and/c hash? (not/c immutable?))"
                            table))
    (let loop ()
      (define i (hash-iterate-first table))
      (when i
        (hash-remove! table (hash-iterate-key table i))
        (loop))))

  (define (hash-clear table)
    (unless (hash? table)
      (raise-argument-error 'hash-clear "hash?" table))
    (cond
      [(immutable? table)
       (cond
         [(hash-equal? table) (hash)]
         [(hash-eqv? table) (hasheqv)]
         [(hash-eq? table) (hasheq)])]
      [(hash-weak? table)
       (cond
         [(hash-equal? table) (make-weak-hash)]
         [(hash-eqv? table) (make-weak-hasheqv)]
         [(hash-eq? table) (make-weak-hasheq)])]
      [else
       (cond
         [(hash-equal? table) (make-hash)]
         [(hash-eqv? table) (make-hasheqv)]
         [(hash-eq? table) (make-hasheq)])]))

  (define (hash-empty? table)
    (unless (hash? table)
      (raise-argument-error 'hash-empty? "hash?" table))
    (zero? (hash-count table)))

  (provide hash-keys
           hash-values
           hash->list
           hash-set*
           hash-set*!
           hash-empty?
           hash-clear
           hash-clear!))
