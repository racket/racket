(module hash "pre-base.rkt"
  (require '#%unsafe
           (for-syntax "ellipses.rkt"))

  (define-syntax-rule (define/optional-try-order (name table try-order?)
                        body0 body ...)
    (define name
      (let* ([name (λ (table try-order?)
                     body0 body ...)]
             [name
              (case-lambda
                [(table)
                 (unless (hash? table)
                   (raise-argument-error 'name "hash?" 0 table))
                 (name table #f)]
                [(table try-order?)
                 (unless (hash? table)
                   (raise-argument-error 'name "hash?" 0 table try-order?))
                 (name table try-order?)])])
        name)))

  (define/optional-try-order (hash-keys h try-order?)
    (if try-order?
        (hash-map h (λ (k v) k) #t)
        (let loop ([pos (hash-iterate-first h)])
          (if pos
              (let ([k (hash-iterate-key h pos unsafe-undefined)]
                    [r (loop (hash-iterate-next h pos))])
                (if (eq? k unsafe-undefined)
                    r
                    (cons k r)))
              null))))

  (define/optional-try-order (hash-values h try-order?)
    (hash-map h (λ (k v) v) try-order?))

  (define/optional-try-order (hash->list h try-order?)
    (hash-map h cons try-order?))

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

  (define (hash-copy-clear table #:kind [kind #f])
    (unless (hash? table)
      (raise-argument-error 'hash-copy-clear "hash?" table))
    (unless (memq kind '(#f immutable mutable weak ephemeron))
      (raise-argument-error
       'hash-copy-clear
       "(or/c #f 'immutable 'mutable 'weak 'ephemeron)"
       kind))
    (cond
     [(if kind (eq? 'immutable kind) (immutable? table))
      (cond
       [(hash-equal? table) (hash)]
       [(hash-equal-always? table) (hashalw)]
       [(hash-eqv? table) (hasheqv)]
       [(hash-eq? table) (hasheq)])]
     [(if kind (eq? 'weak kind) (hash-weak? table))
      (cond
       [(hash-equal? table) (make-weak-hash)]
       [(hash-equal-always? table) (make-weak-hashalw)]
       [(hash-eqv? table) (make-weak-hasheqv)]
       [(hash-eq? table) (make-weak-hasheq)])]
     [(if kind (eq? 'ephemeron kind) (hash-ephemeron? table))
      (cond
        [(hash-equal? table) (make-ephemeron-hash)]
        [(hash-equal-always? table) (make-ephemeron-hashalw)]
        [(hash-eqv? table) (make-ephemeron-hasheqv)]
        [(hash-eq? table) (make-ephemeron-hasheq)])]
     [else
      (cond
       [(hash-equal? table) (make-hash)]
       [(hash-equal-always? table) (make-hashalw)]
       [(hash-eqv? table) (make-hasheqv)]
       [(hash-eq? table) (make-hasheq)])]))

  (define (hash-map/copy table f #:kind [kind #f])
    (unless (hash? table)
      (raise-argument-error 'hash-map/copy "hash?" table))
    (unless (and (procedure? f) (procedure-arity-includes? f 2))
      (raise-argument-error 'hash-map/copy "(procedure-arity-includes/c 2)" f))
    (unless (memq kind '(#f immutable mutable weak ephemeron))
      (raise-argument-error
       'hash-map/copy
       "(or/c #f 'immutable 'mutable 'weak 'ephemeron)"
       kind))
    (define acc (hash-copy-clear table #:kind kind))
    (cond
     [(immutable? acc)
      (for/fold ([acc acc])
                ([(k1 v1) (in-hash table)])
        (define-values [k2 v2] (f k1 v1))
        (hash-set acc k2 v2))]
     [else
      (for ([(k1 v1) (in-hash table)])
        (define-values [k2 v2] (f k1 v1))
        (hash-set! acc k2 v2))
      acc]))

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
           hash-copy-clear
           hash-map/copy))
