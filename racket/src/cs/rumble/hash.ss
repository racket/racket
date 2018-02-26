;; To support iteration and locking, we wrap Chez's mutable hash
;; tables in a `mutable-hash` record:
(define-record mutable-hash (ht           ; Chez Scheme hashtable
                             keys         ; vector of keys for iteration
                             keys-removed ; 'check or a weak, `eqv?`-based mapping of `keys` values
                             lock))
(define (create-mutable-hash ht kind) (make-mutable-hash ht #f #f (make-lock kind)))

(define (authentic-hash? v) (or (intmap? v) (mutable-hash? v) (weak-equal-hash? v)))
(define (hash? v) (or (authentic-hash? v)
                      (and (impersonator? v)
                           (authentic-hash? (impersonator-val v)))))

(define make-hash
  (case-lambda
   [() (create-mutable-hash (make-hashtable key-equal-hash-code key-equal?) 'equal?)]
   [(alist) (fill-hash! 'make-hash (make-hash) alist)]))

(define make-hasheq
  (case-lambda
   [() (create-mutable-hash (make-eq-hashtable) 'eq?)]
   [(alist) (fill-hash! 'make-hasheq (make-hasheq) alist)]))

(define make-weak-hasheq
  (case-lambda
   [() (create-mutable-hash (make-weak-eq-hashtable) 'eq?)]
   [(alist) (fill-hash! 'make-weak-hasheq (make-weak-hasheq) alist)]))

(define make-hasheqv
  (case-lambda
   [() (create-mutable-hash (make-eqv-hashtable) 'eqv?)]
   [(alist) (fill-hash! 'make-hasheqv (make-hasheqv) alist)]))

(define make-weak-hasheqv
  (case-lambda
   [() (create-mutable-hash (make-weak-eqv-hashtable) 'eqv?)]
   [(alist) (fill-hash! 'make-weak-hasheqv (make-weak-hasheqv) alist)]))

(define/who (fill-hash! who ht alist)
  (check who :test (and (list? alist) (andmap pair? alist)) :contract "(listof pair?)" alist)
  (for-each (lambda (p)
              (hash-set! ht (car p) (cdr p)))
            alist)
  ht)

(define-syntax define-hash-constructors
  (syntax-rules ()
    [(_ vararg-ctor list-ctor empty-hash)
     (begin
       (define (vararg-ctor . kvs)
         (let loop ([kvs kvs] [h empty-hash])
           (cond [(null? kvs) h]
                 [else
                  (loop (cddr kvs) (intmap-set h (car kvs) (cadr kvs)))])))

       (define list-ctor
         (case-lambda
          [() (vararg-ctor)]
          [(alist)
           (check 'list-ctor
                  :test (and (list? alist) (andmap pair? alist))
                  :contract "(listof pair?)"
                  alist)
           (let loop ([h (vararg-ctor)] [alist alist])
             (if (null? alist)
                 h
                 (loop (intmap-set h (caar alist) (cdar alist))
                       (cdr alist))))])))]))

(define-hash-constructors hash make-immutable-hash empty-hash)
(define-hash-constructors hasheqv make-immutable-hasheqv empty-hasheqv)
(define-hash-constructors hasheq make-immutable-hasheq empty-hasheq)

(define (hash-set! ht k v)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (when (and (mutable-hash-keys ht)
               (not (hashtable-contains? (mutable-hash-ht ht) k)))
      (set-mutable-hash-keys! ht #f)
      (set-mutable-hash-keys-removed! ht #f))
    (hashtable-set! (mutable-hash-ht ht) k v)
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-set! ht k v)]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (or (mutable-hash? ht)
               (weak-equal-hash? ht))))
    (impersonate-hash-set! ht k v)]
   [else (raise-argument-error 'hash-set! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-remove! ht k)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (when (mutable-hash-keys ht)
      (cond
       [(hash-equal? ht)
        ;; Track which keys in the vector are no longer mapped
        (unless (mutable-hash-keys-removed ht)
          ;; We use an `eqv?` table to work with flonums
          (set-mutable-hash-keys-removed! ht (make-weak-eqv-hashtable)))
        ;; Get specific key that is currently mapped for `k`
        ;; by getting the entry pair:
        (let ([e (hashtable-cell (mutable-hash-ht ht) k #f)])
          (hashtable-set! (mutable-hash-keys-removed ht) (car e) #t))]
       [else
        ; Record that we need to check the table:
        (set-mutable-hash-keys-removed! ht 'check)]))
    (hashtable-delete! (mutable-hash-ht ht) k)
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-remove! ht k)]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (or (mutable-hash? ht)
               (weak-equal-hash? ht))))
    (impersonate-hash-remove! ht k)]
   [else (raise-argument-error 'hash-remove! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-clear! ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (set-mutable-hash-keys! ht #f)
    (set-mutable-hash-keys-removed! ht #f)
    (hashtable-clear! (mutable-hash-ht ht))
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-clear! ht)]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (or (mutable-hash? ht)
               (weak-equal-hash? ht))))
    (impersonate-hash-clear! ht)]
   [else (raise-argument-error 'hash-clear! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-copy ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (let ([new-ht (create-mutable-hash (hashtable-copy (mutable-hash-ht ht) #t)
                                       (cond
                                        [(hash-eq? ht) 'eq?]
                                        [(hash-eqv? ht) 'eqv?]
                                        [else 'equal?]))])
      (lock-release (mutable-hash-lock ht))
      new-ht)]
   [(weak-equal-hash? ht) (weak-hash-copy ht)]
   [(intmap? ht)
    (let ([new-ht (cond
                   [(intmap-eq? ht) (make-hasheq)]
                   [(intmap-eqv? ht) (make-hasheqv)]
                   [else (make-hash)])])
      (let loop ([i (intmap-iterate-first ht)])
        (when i
          (let-values ([(k v) (intmap-iterate-key+value ht i #f)])
            (hash-set! new-ht k v)
            (loop (intmap-iterate-next ht i)))))
      new-ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-copy ht)]
   [else (raise-argument-error 'hash-copy "hash?" ht)]))

(define (hash-set ht k v)
  (cond
   [(intmap? ht) (intmap-set ht k v)]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (impersonate-hash-set ht k v)]
   [else (raise-argument-error 'hash-set! "(and/c hash? immutable?)" ht)]))

(define (hash-remove ht k)
  (cond
   [(intmap? ht) (intmap-remove ht k)]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (impersonate-hash-remove ht k)]
   [else (raise-argument-error 'hash-remove "(and/c hash? immutable?)" ht)]))

(define (hash-clear ht)
  (cond
   [(intmap? ht)
    (cond
     [(hash-eq? ht) empty-hasheq]
     [(hash-eqv? ht) empty-hasheqv]
     [else empty-hash])]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (let loop ([ht ht])
      (let ([i (hash-iterate-first ht)])
        (if i
            (loop (hash-remove ht (hash-iterate-key ht i)))
            ht)))]
   [else (raise-argument-error 'hash-clear! "(and/c hash? immutable?)" ht)]))

(define (hash-eq? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) eq?)]
   [(intmap? ht)
    (intmap-eq? ht)]
   [(weak-equal-hash? ht) #f]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-eq? (impersonator-val ht))]
   [else (raise-argument-error 'hash-eq? "hash?" ht)]))

(define (hash-eqv? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) eqv?)]
   [(intmap? ht)
    (intmap-eqv? ht)]
   [(weak-equal-hash? ht) #f]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-eqv? (impersonator-val ht))]
   [else (raise-argument-error 'hash-eqv? "hash?" ht)]))

(define (hash-equal? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) key-equal?)]
   [(intmap? ht)
    (intmap-equal? ht)]
   [(weak-equal-hash? ht) #t]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-equal? (impersonator-val ht))]
   [else (raise-argument-error 'hash-equal? "hash?" ht)]))

(define (hash-weak? ht)
  (cond
   [(mutable-hash? ht)
    (hashtable-weak? (mutable-hash-ht ht))]
   [(intmap? ht) #f]
   [(weak-equal-hash? ht) #t]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-weak? (impersonator-val ht))]
   [else (raise-argument-error 'hash-weak? "hash?" ht)]))

(define hash-ref
  (case-lambda
    [(ht k)
     (let ([v (hash-ref ht k none)])
       (if (eq? v none)
           (raise-arguments-error
            'hash-ref
            "no value found for key"
            "key" k)
           v))]
    [(ht k fail)
     (cond
      [(mutable-hash? ht)
       (lock-acquire (mutable-hash-lock ht))
       (let ([v (hashtable-ref (mutable-hash-ht ht) k none)])
         (lock-release (mutable-hash-lock ht))
         (if (eq? v none)
             (if (procedure? fail)
                 (|#%app| fail)
                 fail)
             v))]
      [(intmap? ht) (intmap-ref ht k fail)]
      [(weak-equal-hash? ht) (weak-hash-ref ht k fail)]
      [(and (impersonator? ht)
            (authentic-hash? (impersonator-val ht)))
       (let ([v (impersonate-hash-ref ht k)])
         (if (eq? v none)
             (if (procedure? fail)
                 (|#%app| fail)
                 fail)
             v))]
      [else (raise-argument-error 'hash-ref "hash?" ht)])]))

(define/who hash-for-each
  (case-lambda
   [(ht proc) (hash-for-each ht proc #f)]
   [(ht proc try-order?)
    (check who hash? ht)
    (check who (procedure-arity-includes/c 2) proc)
    (cond
     [(mutable-hash? ht)
      (let loop ([i (hash-iterate-first ht)])
        (when i
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (|#%app| proc key val))
          (loop (hash-iterate-next ht i))))]
     [(intmap? ht) (intmap-for-each ht proc)]
     [(weak-equal-hash? ht) (weak-hash-for-each ht proc)]
     [else
      ;; impersonated
      (let loop ([i (hash-iterate-first ht)])
        (when i
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (|#%app| proc key val)
            (loop (hash-iterate-next ht i)))))])]))

(define/who hash-map
  (case-lambda
   [(ht proc)
    (check who hash? ht)
    (check who (procedure-arity-includes/c 2) proc)
    (cond
     [(mutable-hash? ht)
      (let loop ([i (hash-iterate-first ht)])
        (if (not i)
            '()
            (cons
             (let-values ([(key val) (hash-iterate-key+value ht i)])
               (|#%app| proc key val))
             (loop (hash-iterate-next ht i)))))]
     [(intmap? ht) (intmap-map ht proc)]
     [(weak-equal-hash? ht) (weak-hash-map ht proc)]
     [else
      ;; impersonated
      (let loop ([i (hash-iterate-first ht)])
        (cond
         [(not i) '()]
         [else
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (cons (|#%app| proc key val)
                  (loop (hash-iterate-next ht i))))]))])]
   [(ht proc try-order?)
    (hash-map ht proc)]))

(define (hash-count ht)
  (cond
   [(mutable-hash? ht) (hashtable-size (mutable-hash-ht ht))]
   [(intmap? ht) (intmap-count ht)]
   [(weak-equal-hash? ht) (weak-hash-count ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-count (impersonator-val ht))]
   [else (raise-argument-error 'hash-count "hash?" ht)]))

(define (hash-keys-subset? ht1 ht2)
  (cond
   [(and (intmap? ht1)
         (intmap? ht2)
         (or (and (intmap-eq? ht1)
                  (intmap-eq? ht2))
             (and (intmap-eqv? ht1)
                  (intmap-eqv? ht2))
             (and (intmap-equal? ht1)
                  (intmap-equal? ht2))))
    (intmap-keys-subset? ht1 ht2)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2))))
    (and (<= (hash-count ht1) (hash-count ht2))
         (let ([ok? #t])
           (hash-for-each
            ht1
            (lambda (k v)
              (when ok?
                (set! ok? (not (eq? none (hash-ref ht2 k none)))))))
           ok?))]
   [(not (hash? ht1))
    (raise-argument-error 'hash-keys-subset? "hash?" ht1)]
   [(not (hash? ht2))
    (raise-argument-error 'hash-keys-subset? "hash?" ht2)]
   [else
    (raise-arguments-error 'hash-keys-subset?
                           "given hash tables do not use the same key comparison"
                           "first table" ht1
                           "first table" ht2)]))

;; Use `eql?` for recursive comparisons
(define (hash=? ht1 ht2 eql?)
  (cond
   [(and (intmap? ht1)
         (intmap? ht2))
    (intmap=? ht1 ht2 eql?)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2)))
         (eq? (hash-weak? ht1) (hash-weak? ht2)))
    (and (= (hash-count ht1) (hash-count ht2))
         ;; This generic comparison supports impersonators
         (let loop ([i (hash-iterate-first ht1)])
           (cond
            [(not i) #t]
            [else
             (let-values ([(key val) (hash-iterate-key+value ht1 i)])
               (let ([val2 (hash-ref ht2 key none)])
                 (cond
                  [(eq? val2 none) #f]
                  [else (and (eql? val val2)
                             (loop (hash-iterate-next ht1 i)))])))])))]
   [else #f]))


;; Use `hash` for recursive hashing
(define (hash-hash-code ht hash)
  (cond
   [(intmap? ht) (intmap-hash-code ht hash)]
   [else
    ;; This generic hashing supports impersonators
    (let loop ([hc 0] [i (hash-iterate-first ht)])
      (cond
       [(not i) hc]
       [else
        (let* ([eq-key? (hash-eq? ht)]
               [eqv-key? (and (not eq?) (hash-eqv? ht))])
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (let ([hc (hash-code-combine-unordered hc
                                                   (cond
                                                    [eq-key? (eq-hash-code key)]
                                                    [eqv-key? (eqv-hash-code key)]
                                                    [else (hash key)]))])
              (loop (hash-code-combine-unordered hc (hash val))
                    (hash-iterate-next ht i)))))]))]))


;; A `hash-iterate-first` operation triggers an O(n)
;; gathering of the keys of a mutable hash table. That's
;; unfortunate, but there appears to be no way around it.
(define (prepare-iterate! ht i)
  (lock-acquire (mutable-hash-lock ht))
  (let ([vec (mutable-hash-keys ht)])
    (cond
     [vec
      (lock-release (mutable-hash-lock ht))
      vec]
     [else
      (let ([vec (hashtable-keys (mutable-hash-ht ht))])
        ;; Keep a weak reference to each key, in case
        ;; it's removed or we have a weak hash table:
        (let loop ([i (vector-length vec)])
          (unless (zero? i)
            (let* ([i (sub1 i)]
                   [key (vector-ref vec i)])
              (vector-set! vec i (weak/fl-cons key #f))
              (loop i))))
        (set-mutable-hash-keys! ht vec)
        (set-mutable-hash-keys-removed! ht #f)
        (lock-release (mutable-hash-lock ht))
        vec)])))

(define/who (hash-iterate-first ht)
  (cond
   [(intmap? ht)
    (intmap-iterate-first ht)]
   [(mutable-hash? ht)
    (mutable-hash-iterate-next ht #f)]
   [(weak-equal-hash? ht) (weak-hash-iterate-first ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    ;; `hash-iterate-first` must not hash any keys:
    (hash-iterate-first (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define (check-i who i)
  (check who exact-nonnegative-integer? i))

(define/who (hash-iterate-next ht i)
  (cond
   [(intmap? ht)
    (check-i 'hash-iterate-next i)
    (intmap-iterate-next ht i)]
   [(mutable-hash? ht)
    (check-i 'hash-iterate-next i)
    (mutable-hash-iterate-next ht i)]
   [(weak-equal-hash? ht)
    (check-i 'hash-iterate-next i)
    (weak-hash-iterate-next ht i)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    ;; `hash-iterate-next` must not hash any keys:
    (hash-iterate-next (impersonator-val ht) i)]
   [else (raise-argument-error who "hash?" ht)]))

(define (mutable-hash-iterate-next ht init-i)
  (let* ([vec (prepare-iterate! ht init-i)] ; vec expected to have > `init-i` elements
         [len (vector-length vec)])
    (let loop ([i (or init-i -1)])
      (let ([i (add1 i)])
        (cond
         [(> i len)
          (raise-arguments-error 'hash-iterate-next "no element at index"
                                 "index" init-i
                                 "within length" len
                                 "vec" vec)]
         [(= i len)
          #f]
         [else
          (let* ([p (vector-ref vec i)]
                 [key (car p)])
            (cond
             [(bwp-object? key)
              ;; A hash table change or disappeared weak reference
              (loop i)]
             [(mutable-hash-keys-removed ht)
              => (lambda (keys-removed)
                   (lock-acquire (mutable-hash-lock ht))
                   (let ([removed?
                          (if (eq? keys-removed 'check)
                              (not (hashtable-contains? (mutable-hash-ht ht) key))
                              (hashtable-contains? keys-removed key))])
                     (lock-release (mutable-hash-lock ht))
                     (if removed?
                         ;; Skip, due to a hash table change
                         (loop i)
                         ;; Key is still mapped:
                         i)))]
             [else i]))])))))

(define (do-hash-iterate-key+value who ht i
                                   intmap-iterate-key+value
                                   weak-hash-iterate-key+value
                                   key? value? pair?)
  (cond
   [(intmap? ht)
    (check-i who i)
    (call-with-values (lambda () (intmap-iterate-key+value ht i none))
      (case-lambda
        [(v) (if (eq? v none)
                 (raise-arguments-error who "no element at index"
                                        "index" i)
                 v)]
        [(k v) (values k v)]))]
   [(mutable-hash? ht)
    (check-i who i)
    (let* ([vec (prepare-iterate! ht i)]
           [len (vector-length vec)]
           [p (if (< i len)
                  (vector-ref vec i)
                  '(#f . #f))]
           [key (car p)]
           [v (if (bwp-object? key)
                  none
                  (cond
                   [(not value?)
                    ;; We need to check whether the key is still
                    ;; mapped by the hash table, but impersonator
                    ;; support relies on not `equal?`-hashing the
                    ;; candidate key at this point. The `keys-removed`
                    ;; weak `eq?`-based table serves that purpose.
                    (cond
                     [(mutable-hash-keys-removed ht)
                      => (lambda (keys-removed)
                           (lock-acquire (mutable-hash-lock ht))
                           (let ([removed?
                                  (if (eq? keys-removed 'check)
                                      (not (hashtable-contains? (mutable-hash-ht ht) key))
                                      (hashtable-contains? keys-removed key))])
                             (lock-release (mutable-hash-lock ht))
                             (if removed? none #t)))]
                     [else #t])]
                   [else
                    (lock-acquire (mutable-hash-lock ht))
                    (let ([v (hashtable-ref (mutable-hash-ht ht) key none)])
                      (lock-release (mutable-hash-lock ht))
                      v)]))])
      (if (eq? v none)
          (raise-arguments-error who "no element at index"
                                 "index" i)
          (cond
           [(and key? value?)
            (if pair?
                (cons key v)
                (values key v))]
           [key? key]
           [else v])))]
   [(weak-equal-hash? ht)
    (check-i who i)
    (weak-hash-iterate-key+value ht i)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-iterate-key+value who ht i key? value? pair?)]
   [else (raise-argument-error who "hash?" ht)]))

(define (hash-iterate-key ht i)
  (do-hash-iterate-key+value 'hash-iterate-key ht i
                             intmap-iterate-key
                             weak-hash-iterate-key
                             #t #f #f))

(define (hash-iterate-value ht i)
  (do-hash-iterate-key+value 'hash-iterate-value ht i
                             intmap-iterate-value
                             weak-hash-iterate-value
                             #f #t #f))

(define (hash-iterate-key+value ht i)
  (do-hash-iterate-key+value 'hash-iterate-key+value ht i
                             intmap-iterate-key+value
                             weak-hash-iterate-key+value
                             #t #t #f))

(define (hash-iterate-pair ht i)
  (do-hash-iterate-key+value 'hash-iterate-pair ht i
                             intmap-iterate-pair
                             weak-hash-iterate-pair
                             #t #t #t))

(define (unsafe-immutable-hash-iterate-first ht)
  (if (impersonator? ht)
      (hash-iterate-first ht)
      (unsafe-intmap-iterate-first ht)))

(define (iterator-for-impersonator? i) (fixnum? i))

(define (unsafe-immutable-hash-iterate-next ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-next ht i)
      (unsafe-intmap-iterate-next ht i)))

(define (unsafe-immutable-hash-iterate-key ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-key ht i)
      (unsafe-intmap-iterate-key ht i)))

(define (unsafe-immutable-hash-iterate-value ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-value ht i)
      (unsafe-intmap-iterate-value ht i)))

(define (unsafe-immutable-hash-iterate-key+value ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-key+value ht i)
      (unsafe-intmap-iterate-key+value ht i)))

(define (unsafe-immutable-hash-iterate-pair ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-pair ht i)
      (unsafe-intmap-iterate-pair ht i)))

(define unsafe-mutable-hash-iterate-first hash-iterate-first)
(define unsafe-mutable-hash-iterate-next hash-iterate-next)
(define unsafe-mutable-hash-iterate-key hash-iterate-key)
(define unsafe-mutable-hash-iterate-value hash-iterate-value)
(define unsafe-mutable-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-mutable-hash-iterate-pair hash-iterate-pair)

(define unsafe-weak-hash-iterate-first hash-iterate-first)
(define unsafe-weak-hash-iterate-next hash-iterate-next)
(define unsafe-weak-hash-iterate-key hash-iterate-key)
(define unsafe-weak-hash-iterate-value hash-iterate-value)
(define unsafe-weak-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-weak-hash-iterate-pair hash-iterate-pair)

;;  ----------------------------------------

;; Chez Scheme doesn't provide weak hash table with `equal?` comparisons,
;; so build our own

(define-record weak-equal-hash (keys-ht    ; integer[equal hash code] -> weak list of keys
                                vals-ht    ; weak, eq?-based hash table: key -> value
                                count      ; number of items in the table (= sum of list lengths)
                                prune-at   ; count at which we should try to prune empty weak boxes
                                keys))     ; for iteration: a vector that is enlarged on demand

(define make-weak-hash
  (case-lambda
   [() (make-weak-equal-hash (hasheqv) (make-weak-eq-hashtable) 0 128 #f)]
   [(alist) (fill-hash! 'make-weak-hash (make-weak-hash) alist)]))

(define (weak-hash-copy ht)
  (make-weak-equal-hash (weak-equal-hash-keys-ht ht)
                        (hashtable-copy (weak-equal-hash-vals-ht ht) #t)
                        (weak-equal-hash-count ht)
                        (weak-equal-hash-prune-at ht)
                        #f))

(define (weak-hash-ref t key fail)
  (let* ([code (key-equal-hash-code key)]
         [keys (intmap-ref (weak-equal-hash-keys-ht t) code '())])
    (let loop ([keys keys])
      (cond
       [(null? keys)
        ;; Not in the table:
        (if (procedure? fail)
            (|#%app| fail)
            fail)]
       [(key-equal? (car keys) key)
        (let ([v (hashtable-ref (weak-equal-hash-vals-ht t) (car keys) none)])
          (if (eq? v none)
              (if (procedure? fail)
                  (|#%app| fail)
                  fail)
              v))]
       [else (loop (cdr keys))]))))

(define (weak-hash-ref-key ht key)
  (let* ([code (key-equal-hash-code key)]
         [keys (intmap-ref (weak-equal-hash-keys-ht ht) code '())])
    (let loop ([keys keys])
      (cond
       [(null? keys) #f]
       [(key-equal? (car keys) key) (car keys)]
       [else (loop (cdr keys))]))))

(define (weak-hash-set! t k v)
  (let* ([code (key-equal-hash-code k)]
         [keys (intmap-ref (weak-equal-hash-keys-ht t) code '())])
    (let loop ([keys keys])
      (cond
       [(null? keys)
        ;; Not in the table:
        (set-weak-equal-hash-keys! t #f)
        (when (= (weak-equal-hash-count t) (weak-equal-hash-prune-at t))
          (prune-table! t))
        (let* ([ht (weak-equal-hash-keys-ht t)])
          (set-weak-equal-hash-count! t
                                      (add1 (weak-equal-hash-count t)))
          (set-weak-equal-hash-keys-ht! t
                                        (intmap-set ht code
                                                    (weak/fl-cons k
                                                                  (intmap-ref ht code '()))))
          (hashtable-set! (weak-equal-hash-vals-ht t) k v))]
       [(key-equal? (car keys) k)
        (hashtable-set! (weak-equal-hash-vals-ht t) (car keys) v)]
       [else (loop (cdr keys))]))))

(define (weak-hash-remove! t k)
  (let* ([code (key-equal-hash-code k)]
         [keys (intmap-ref (weak-equal-hash-keys-ht t) code '())]
         [keep-bwp?
          ;; If we have a `keys` array, then preserve the shape of
          ;; each key lst in `(weak-equal-hash-keys-ht t)` so that
          ;; the `keys` array remains consistent with that shape
          (and (weak-equal-hash-keys t) #t)]
         [new-keys
          (let loop ([keys keys])
            (cond
             [(null? keys)
              ;; Not in the table
              #f]
             [(key-equal? (car keys) k)
              (hashtable-delete! (weak-equal-hash-vals-ht t) (car keys))
              (if keep-bwp?
                  (cons #!bwp keys)
                  (cdr keys))]
             [else
              (let ([new-keys (loop (cdr keys))])
                (and new-keys
                     (if (and (not keep-bwp?)
                              (bwp-object? (car keys)))
                         new-keys
                         (weak/fl-cons (car keys) new-keys))))]))])
    (when new-keys
      (set-weak-equal-hash-keys-ht! t
                                    (if (null? new-keys)
                                        (intmap-remove (weak-equal-hash-keys-ht t) code)
                                        (intmap-set (weak-equal-hash-keys-ht t) code new-keys))))))

(define (weak-hash-clear! t)
  (set-weak-equal-hash-keys-ht! t (hasheqv))
  (hashtable-clear! (weak-equal-hash-vals-ht t))
  (set-weak-equal-hash-count! t 0)
  (set-weak-equal-hash-prune-at! t 128)
  (set-weak-equal-hash-keys! t #f))

(define (weak-hash-for-each t proc)
  (let* ([ht (weak-equal-hash-vals-ht t)]
         [keys (hashtable-keys ht)]
         [len (#%vector-length keys)])
    (let loop ([i 0])
      (unless (fx= i len)
        (let ([key (#%vector-ref keys i)])
          (|#%app| proc key (hashtable-ref ht key #f)))
        (loop (fx1+ i))))))

(define (weak-hash-map t proc)
  (let* ([ht (weak-equal-hash-vals-ht t)]
         [keys (hashtable-keys ht)]
         [len (#%vector-length keys)])
    (let loop ([i 0])
      (cond
       [(fx= i len) '()]
       [else
        (let ([key (#%vector-ref keys i)])
          (cons (|#%app| proc key (hashtable-ref ht key #f))
                (loop (fx1+ i))))]))))

(define (weak-hash-count t)
  (hashtable-size (weak-equal-hash-vals-ht t)))

(define (prepare-weak-iterate! ht i)
  (let* ([current-vec (weak-equal-hash-keys ht)])
    (or (and current-vec
             (> (vector-length current-vec) (or i 1))
             current-vec)
        (let* ([len (max 16
                         (* 2 (if current-vec
                                  (vector-length current-vec)
                                  0))
                         (if i (* 2 i) 0))]
               [vec (make-vector len #f)]
               [pos (box 0)])
          (call/cc
           (lambda (esc)
             (intmap-for-each
              (weak-equal-hash-keys-ht ht)
              (lambda (k l)
                (let loop ([l l])
                  (cond
                   [(null? l) (void)]
                   [else
                    ;; Add `l` even if the key is #!bwp,
                    ;; so that iteration works right if a key
                    ;; is removed
                    (vector-set! vec (unbox pos) l)
                    (set-box! pos (add1 (unbox pos)))
                    (if (= (unbox pos) len)
                        ;; That's enough keys
                        (esc (void))
                        (loop (cdr l)))]))))))
          (set-weak-equal-hash-keys! ht vec)
          vec))))

(define (weak-hash-iterate-first ht)
  (weak-hash-iterate-next ht #f))

(define (weak-hash-iterate-next ht init-i)
  (let retry ([i (and init-i (add1 init-i))])
    (let* ([vec (prepare-weak-iterate! ht i)]
           [len (vector-length vec)])
      (let loop ([i (or i 0)])
        (cond
         [(= i len)
          ;; expand set of prepared keys
          (retry i)]
         [(> i len)
          (raise-arguments-error 'hash-iterate-next "no element at weak index"
                                 "index" init-i)]
         [else
          (let ([p (vector-ref vec i)])
            (cond
             [(not p)
              ;; no more keys available
              #f]
             [(bwp-object? (car p)) (loop (add1 i))]
             [(not (hashtable-contains? (weak-equal-hash-vals-ht ht) (car p)))
              ;; key was removed from table after `keys` array was formed
              (loop (add1 i))]
             [else i]))])))))

(define (do-weak-hash-iterate-key who ht i)
  (let* ([vec (weak-equal-hash-keys ht)]
         [p (and vec
                 (< i (vector-length vec))
                 (vector-ref vec i))]
         [k (if p
                (car p)
                #!bwp)])
    (cond
     [(bwp-object? k)
      (raise-arguments-error who "no element at index"
                             "index" i)]
     [else k])))

(define (weak-hash-iterate-key ht i)
  (do-weak-hash-iterate-key 'hash-iterate-key ht i))

(define (weak-hash-iterate-value ht i)
  (let* ([key (do-weak-hash-iterate-key 'hash-iterate-value ht i)]
         [val (hashtable-ref (weak-equal-hash-vals-ht ht) key none)])
    (if (eq? val none)
        (raise-arguments-error
         'weak-hash-iterate-value "no element at index"
         "index" i)
        val)))

(define (weak-hash-iterate-key+value ht i)
  (let ([key (do-weak-hash-iterate-key 'hash-iterate-key+value ht i)])
    (values key
            (let ([val (hashtable-ref (weak-equal-hash-vals-ht ht) key none)])
              (if (eq? val none)
                  (raise-arguments-error
                   'weak-hash-iterate-key+value "no element at index"
                   "index" i)
                  val)))))

(define (weak-hash-iterate-pair ht i)
  (let ([key (do-weak-hash-iterate-key 'hash-iterate-pair ht i)])
    (cons key
          (let ([val (hashtable-ref (weak-equal-hash-vals-ht ht) key none)])
            (if (eq? val none)
                (raise-arguments-error
                 'weak-hash-iterate-paur "no element at index"
                 "index" i)
                val)))))

;; Remove empty weak boxes from a table. Count the number
;; of remaining entries, and remember to prune again when
;; the number of entries doubles (up to at least reaches 128)
(define (prune-table! t)
  (let ([ht (weak-equal-hash-keys-ht t)])
    (let-values ([(new-ht count)
                  (let loop ([ht ht]
                             [i (intmap-iterate-first ht)]
                             [count 0])
                    (cond
                     [(not i) (values ht count)]
                     [else
                      (let-values ([(key l) (intmap-iterate-key+value ht i #f)])
                        (let ([l (let loop ([l l])
                                   (cond
                                    [(null? l) l]
                                    [(bwp-object? (car l)) (loop (cdr l))]
                                    [else (weak/fl-cons (car l) (loop (cdr l)))]))])
                          (loop (if (null? l)
                                    ht
                                    (hash-set ht key l))
                                (intmap-iterate-next ht i)
                                (+ count (length l)))))]))])
      (set-weak-equal-hash-keys-ht! t new-ht)
      (set-weak-equal-hash-count! t count)
      (set-weak-equal-hash-prune-at! t (max 128 (* 2 count))))))

;; ----------------------------------------

(define (weak/fl-cons key d)
  ;; Special case for flonums, which are never retained in weak pairs,
  ;; but we want to treat them like fixnums and other immediates:
  (if (flonum? key)
      (cons key d)
      (weak-cons key d)))

;; ----------------------------------------

(define (set-hash-hash!)
  (record-type-equal-procedure (record-type-descriptor mutable-hash)
                               hash=?)
  (record-type-hash-procedure (record-type-descriptor mutable-hash)
                              hash-hash-code)
  (record-type-equal-procedure (record-type-descriptor weak-equal-hash)
                               hash=?)
  (record-type-hash-procedure (record-type-descriptor weak-equal-hash)
                              hash-hash-code)

  (record-type-hash-procedure (record-type-descriptor hash-impersonator)
                              hash-hash-code)
  (record-type-hash-procedure (record-type-descriptor hash-chaperone)
                              hash-hash-code))

;; ----------------------------------------

;; `eq?` identity of a `hash-procs` instance matters for
;; `impersonator-of?` and `chaperone-of?`:
(define-record hash-procs (ref set remove key clear equal-key))

(define-record hash-impersonator impersonator (procs))
(define-record hash-chaperone chaperone (procs))

(define/who (impersonate-hash ht ref set remove key . args)
  (check who
         (lambda (p) (let ([p (strip-impersonator p)])
                       (or (mutable-hash? p) (weak-equal-hash? p))))
         :contract "(and/c hash? (not/c immutable?))"
         ht)
  (do-impersonate-hash who ht ref set remove key args
                       make-hash-impersonator))

(define/who (chaperone-hash ht ref set remove key . args)
  (check who hash? ht)
  (do-impersonate-hash who ht ref set remove key args
                       make-hash-chaperone))

(define (do-impersonate-hash who ht ref set remove key args
                             make-hash-chaperone)
  (check who (procedure-arity-includes/c 2) ref)
  (check who (procedure-arity-includes/c 3) set)
  (check who (procedure-arity-includes/c 2) remove)
  (check who (procedure-arity-includes/c 2) key)
  (let* ([clear-given? (and (pair? args)
                            (or (not (car args))
                                (and (procedure? (car args))
                                     (procedure-arity-includes? (car args) 1))))]
         [clear (if clear-given? (car args) void)]
         [args (if clear-given? (cdr args) args)]
         [equal-key-given? (and (pair? args)
                                (or (not (car args))
                                    (and (procedure? (car args))
                                         (procedure-arity-includes? (car args) 2))))]
         [equal-key (if equal-key-given?
                        (car args)
                        (lambda (ht k) k))]
         [args (if equal-key-given? (cdr args) args)])
    (make-hash-chaperone (strip-impersonator ht)
                         ht
                         (add-impersonator-properties who
                                                      args
                                                      (if (impersonator? ht)
                                                          (impersonator-props ht)
                                                          empty-hasheq))
                         (make-hash-procs ref set remove key clear equal-key))))

;; ----------------------------------------

(define (impersonate-hash-ref ht k)
  (impersonate-hash-ref/set 'hash-ref #f
                            (lambda (ht k v) (hash-ref ht k none))
                            (lambda (procs ht k none-v)
                              ((hash-procs-ref procs) ht k))
                            hash-procs-ref
                            ht k none))

(define (impersonate-hash-set! ht k v)
  (impersonate-hash-ref/set 'hash-set! #t
                            hash-set!
                            (lambda (procs ht k v)
                              ((hash-procs-set procs) ht k v))
                            hash-procs-set
                            ht k v))

(define (impersonate-hash-set ht k v)
  (impersonate-hash-ref/set 'hash-set #t
                            hash-set
                            (lambda (procs ht k v)
                              ((hash-procs-set procs) ht k v))
                            hash-procs-set
                            ht k v))

(define (impersonate-hash-remove! ht k)
  (impersonate-hash-ref/set 'hash-remove! #t
                            (lambda (ht k false-v) (hash-remove! ht k))
                            (lambda (procs ht k false-v)
                              (values ((hash-procs-remove procs) ht k) #f))
                            hash-procs-remove
                            ht k #f))

(define (impersonate-hash-remove ht k)
  (impersonate-hash-ref/set 'hash-remove #t
                            (lambda (ht k false-v) (hash-remove ht k))
                            (lambda (procs ht k false-v)
                              (values ((hash-procs-remove procs) ht k) #f))
                            hash-procs-remove
                            ht k #f))

(define (impersonate-hash-ref/set who set? authentic-op apply-wrapper get-wrapper ht k v)
  (let ([wrap-key? (hash-equal? ht)])
    (let loop ([ht ht] [get-k (and wrap-key? values)] [k k] [v v])
      (cond
       [(or (hash-impersonator? ht)
            (hash-chaperone? ht))
        (let ([chaperone? (hash-chaperone? ht)]
              [procs (if (hash-impersonator? ht)
                         (hash-impersonator-procs ht)
                         (hash-chaperone-procs ht))]
              [next-ht (impersonator-next ht)])
          (let ([get-k (and wrap-key? (extend-get-k who get-k procs next-ht chaperone?))])
            (call-with-values
                (lambda () (apply-wrapper procs next-ht k v))
              (case-lambda
               [(new-k new-v-or-wrap)
                ;; In `ref` mode, `new-v-or-wrap` is a wrapper procedure for the result.
                ;; In `set` mode, `new-v-or-wrap` is a replacement value.
                (when chaperone?
                  (unless (or (not chaperone?) (chaperone-of? new-k k))
                    (raise-chaperone-error who "key" new-k k))
                  (when set?
                    (unless (or (not chaperone?) (chaperone-of? new-v-or-wrap v))
                      (raise-chaperone-error who "value" new-v-or-wrap v))))
                ;; Recur...
                (let ([r (loop next-ht get-k new-k (if set? new-v-or-wrap none))])
                  ;; In `ref` mode, `r` is the result value.
                  ;; In `set` mode, `r` is void or an updated hash table.
                  (cond
                   [(and set? (void? r))
                    (void)]
                   [set?
                    ((if chaperone? make-hash-chaperone make-hash-impersonator)
                     (strip-impersonator r)
                     r
                     (impersonator-props ht)
                     procs)]
                   [(eq? r none) none]
                   [else
                    (let ([new-r (new-v-or-wrap next-ht new-k r)])
                      (when chaperone?
                        (unless (chaperone-of? new-r r)
                          (raise-chaperone-error who "value" new-r r)))
                      new-r)]))]
               [args
                (raise-arguments-error who
                                       (string-append (if chaperone? "chaperone" "impersonator")
                                                      " did not return 2 values")
                                       (string-append (if chaperone? "chaperone" "impersonator")
                                                      " procedure")
                                       (get-wrapper procs)
                                       "number of returned values" (length args))]))))]
       [(impersonator? ht)
        (let ([r (loop (impersonator-next ht) get-k k v)])
          (cond
           [(and set? (void? r))
            (void)]
           [set?
            (rewrap-props-impersonator ht r)]
           [else r]))]
       [else
        (if (and get-k (not (eq? get-k values)))
            (call-with-equality-wrap
             get-k
             k
             (lambda () (authentic-op ht k v)))
            (authentic-op ht k v))]))))

;; Add a layer of interposition on `equal?` and `equal-hash-code`:
(define (extend-get-k who get-k procs next-ht chaperone?)
  (lambda (k)
    (let* ([k (get-k k)]
           [new-k ((hash-procs-equal-key procs) next-ht k)])
      (unless (or (not chaperone?) (chaperone-of? new-k k))
        (raise-chaperone-error who "key" new-k k))
      new-k)))

(define (impersonate-hash-clear! ht)
  (let loop ([ht ht])
    (cond
     [(or (hash-impersonator? ht)
          (hash-chaperone? ht))
      (let ([procs (if (hash-impersonator? ht)
                       (hash-impersonator-procs ht)
                       (hash-chaperone-procs ht))]
            [ht (impersonator-next ht)])
        ((hash-procs-clear procs) ht)
        (loop ht))]
     [(impersonator? ht)
      (loop (impersonator-next ht))]
     [else
      (hash-clear! ht)])))

(define (impersonate-hash-copy ht)
  (let* ([val-ht (impersonator-val ht)]
         [mutable? (mutable-hash? val-ht)]
         [new-ht
          (cond
           [mutable?
            (cond
             [(hash-weak? ht)
              (cond
               [(hash-eq? val-ht) (make-weak-hasheq)]
               [(hash-eqv? val-ht) (make-weak-hasheq)]
               [else (make-weak-hash)])]
             [else
              (cond
               [(hash-eq? val-ht) (make-hasheq)]
               [(hash-eqv? val-ht) (make-hasheq)]
               [else (make-hash)])])]
           [else
            (cond
             [(hash-eq? val-ht) (make-hasheq)]
             [(hash-eqv? val-ht) (make-hasheqv)]
             [else (make-hash)])])])
    (let loop ([i (hash-iterate-first ht)])
      (cond
       [i (let-values ([(key val) (hash-iterate-key+value ht i)])
            (hash-set! new-ht key val)
            (loop (hash-iterate-next ht i)))]
       [else new-ht]))))

(define (impersonate-hash-iterate-key+value who ht i key? value? pair?)
  (let ([key (impersonate-hash-iterate-key who ht i)])
    (cond
     [(not value?) key]
     [else
      (let ([val (hash-ref ht key none)])
        (cond
         [(eq? val none)
          (raise-arguments-error who
                                 (string-append "no value found for post-"
                                                (if (impersonator? ht) "impersonator" "chaperone")
                                                " key")
                                 "key" key)]
         [pair? (cons key val)]
         [key? (values key val)]
         [else val]))])))

(define (impersonate-hash-iterate-key who ht i)
  ;; We don't have to set up `get-k`, because `hash-iterate-key`
  ;; is prohibited from hashing any keys
  (let loop ([ht ht])
    (cond
     [(hash-impersonator? ht)
      (let ([procs (hash-impersonator-procs ht)]
            [ht (impersonator-next ht)])
        ((hash-procs-key procs) ht (loop ht)))]
     [(hash-chaperone? ht)
      (let ([procs (hash-chaperone-procs ht)]
            [ht (impersonator-next ht)])
        (let* ([k (loop ht)]
               [new-k ((hash-procs-key procs) ht k)])
          (unless (chaperone-of? new-k k)
            (raise-chaperone-error who "key" new-k k))
          new-k))]
     [(impersonator? ht)
      (loop (impersonator-next ht))]
     [else
      ;; The same as `hash-iterate-key`, but with the correct `who`:
      (do-hash-iterate-key+value who ht i
                                 intmap-iterate-key
                                 weak-hash-iterate-key
                                 #t #f #f)])))
