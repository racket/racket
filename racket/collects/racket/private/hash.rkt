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

  (define (paired-fold who pairs0 init proc)
    (let loop ([value init] [pairs pairs0])
      (cond
        [(null? pairs) value]
        [(null? (cdr pairs))
         (raise-arguments-error
          who
          (format "expected ~a, but received ~a"
                  "an even number of association elements"
                  "an odd number of association elements")
          "association elements"
          pairs0)]
        [else (loop (proc value (car pairs) (cadr pairs))
                    (cddr pairs))])))

  (define (hash-set* table . pairs)
    (unless (and (hash? table) (immutable? table))
      (raise-argument-error 'hash-set*
                            "(and/c hash? immutable?)"
                            table))
    (paired-fold 'hash-set* pairs table hash-set))

  (define (hash-set*! table . pairs)
    (unless (and (hash? table) (not (immutable? table)))
      (raise-argument-error 'hash-set*!
                            "(and/c hash? (not/c immutable?))"
                            table))
    (paired-fold 'hash-set*! pairs (void)
                (lambda (x k v)
                  (hash-set! table k v))))

  (define (hash-copy-clear table)
    (unless (hash? table)
      (raise-argument-error 'hash-copy-clear "hash?" table))
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

  (provide paired-fold ;; only for dict operations, not for racket/base
           hash-keys
           hash-values
           hash->list
           hash-set*
           hash-set*!
           hash-empty?
           hash-copy-clear))
