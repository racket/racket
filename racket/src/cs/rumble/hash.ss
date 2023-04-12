;; Mutable hash table are safe for engine-based concurrency, but they
;; are not safe for concurrent access across Scheme threads.

;; Mutable hash tables need a lock
;; and an iteration vector
(define-record locked-iterable-hash (lock
                                     cells    ; vector of cells for iteration
                                     retry?)) ; is `cells` maybe incomplete?

;; To support iteration and locking, we wrap Chez's mutable hash
;; tables in a `mutable-hash` record
(define-record mutable-hash locked-iterable-hash
  (ht)) ; Chez Scheme hashtable
(define-record eq-mutable-hash mutable-hash
  ())

(define (create-mutable-hash ht kind) (make-mutable-hash (make-lock kind) #f #t ht))
(define (create-eq-mutable-hash ht) (make-eq-mutable-hash (make-lock 'eq?) #f #t ht))

(define (mutable-hash-lock ht) (locked-iterable-hash-lock ht))
(define (mutable-hash-cells ht) (locked-iterable-hash-cells ht))

(define (authentic-hash? v) (or (intmap? v) (mutable-hash? v)))
(define (hash? v) (or (authentic-hash? v)
                      (and (impersonator? v)
                           (authentic-hash? (impersonator-val v)))))
(define (immutable-hash? v) (or (intmap? v)
                                (and (impersonator? v)
                                     (intmap? (impersonator-val v)))))
(define -mutable-hash? ; exported as `mutable-hash?`
  (|#%name|
   mutable-hash?
   (lambda (v) (or (mutable-hash? v)
                   (and (impersonator? v)
                        (mutable-hash? (impersonator-val v)))))))

(define/who make-hash
  (case-lambda
   [() (create-mutable-hash (make-hashtable key-equal-hash-code key-equal?) 'equal?)]
   [(alist) (fill-hash! who (make-hash) alist)]))

(define/who make-weak-hash
  (case-lambda
   [() (create-mutable-hash (make-weak-hashtable key-equal-hash-code key-equal?) 'equal?)]
   [(alist) (fill-hash! who (make-weak-hash) alist)]))

(define/who make-ephemeron-hash
  (case-lambda
   [() (create-mutable-hash (make-ephemeron-hashtable key-equal-hash-code key-equal?) 'equal?)]
   [(alist) (fill-hash! who (make-ephemeron-hash) alist)]))

(define/who make-hasheq
  (case-lambda
   [() (create-eq-mutable-hash (make-eq-hashtable))]
   [(alist) (fill-hash! who (make-hasheq) alist)]))

(define (eq-hashtable->hash ht)
  (create-eq-mutable-hash ht))
(define (hash->eq-hashtable ht)
  (mutable-hash-ht ht))

(define/who make-weak-hasheq
  (case-lambda
   [() (create-eq-mutable-hash (make-weak-eq-hashtable))]
   [(alist) (fill-hash! who (make-weak-hasheq) alist)]))

(define/who make-ephemeron-hasheq
  (case-lambda
   [() (create-eq-mutable-hash (make-ephemeron-eq-hashtable))]
   [(alist) (fill-hash! who (make-ephemeron-hasheq) alist)]))

(define/who make-hasheqv
  (case-lambda
   [() (create-mutable-hash (make-eqv-hashtable) 'eqv?)]
   [(alist) (fill-hash! who (make-hasheqv) alist)]))

(define/who make-weak-hasheqv
  (case-lambda
   [() (create-mutable-hash (make-weak-eqv-hashtable) 'eqv?)]
   [(alist) (fill-hash! who (make-weak-hasheqv) alist)]))

(define/who make-ephemeron-hasheqv
  (case-lambda
   [() (create-mutable-hash (make-ephemeron-eqv-hashtable) 'eqv?)]
   [(alist) (fill-hash! who (make-ephemeron-hasheqv) alist)]))

;; hashalw is for equal ALWays, first 3 letters of "always" since "equal" is implicit
(define/who make-hashalw
  (case-lambda
   [() (create-mutable-hash (make-hashtable key-equal-always-hash-code key-equal-always?) 'equal-always?)]
   [(alist) (fill-hash! who (make-hashalw) alist)]))

(define/who make-weak-hashalw
  (case-lambda
   [() (create-mutable-hash (make-weak-hashtable key-equal-always-hash-code key-equal-always?) 'equal-always?)]
   [(alist) (fill-hash! who (make-weak-hashalw) alist)]))

(define/who make-ephemeron-hashalw
  (case-lambda
   [() (create-mutable-hash (make-ephemeron-hashtable key-equal-always-hash-code key-equal-always?) 'equal-always?)]
   [(alist) (fill-hash! who (make-ephemeron-hashalw) alist)]))

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
       (define vararg-ctor
         (case-lambda
          [() empty-hash]
          [kvs
           (let loop ([kvs kvs] [h empty-hash])
             (cond
              [(null? kvs) h]
              [(null? (cdr kvs))
               (raise-arguments-error
                'vararg-ctor
                "key does not have a value (i.e., an odd number of arguments were provided)"
                "key" (car kvs))]
              [else (loop (cddr kvs) (intmap-set h (car kvs) (cadr kvs)))]))]))

       (define list-ctor
         (case-lambda
          [() empty-hash]
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
(define-hash-constructors hashalw make-immutable-hashalw empty-hashalw)
(define-hash-constructors hasheqv make-immutable-hasheqv empty-hasheqv)
(define-hash-constructors hasheq make-immutable-hasheq empty-hasheq)

(define (hash-set! ht k v)
  (cond
   [(mutable-hash? ht)
    (cond
     [(and (current-future) (eq-mutable-hash? ht))
      (future-sync 'hash-set! (lambda () (mutable-hash-set! ht k v)))]
     [else (mutable-hash-set! ht k v)])]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (mutable-hash? ht)))
    (impersonate-hash-set! ht k v)]
   [else (raise-argument-error 'hash-set! "(and/c hash? (not/c immutable?))" 0 ht k v)]))

(define (mutable-hash-set! ht k v)
  (lock-acquire (mutable-hash-lock ht))
  (hashtable-set! (mutable-hash-ht ht) k v)
  (set-locked-iterable-hash-retry?! ht #t)
  (lock-release (mutable-hash-lock ht)))

(define (hash-remove! ht k)
  (cond
   [(mutable-hash? ht)
    (cond
     [(and (current-future) (eq-mutable-hash? ht))
      (future-sync 'hash-remove! (lambda () (mutable-hash-remove! ht k)))]
     [else (mutable-hash-remove! ht k)])]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (mutable-hash? ht)))
    (impersonate-hash-remove! ht k)]
   [else (raise-argument-error 'hash-remove! "(and/c hash? (not/c immutable?))" 0 ht k)]))

(define (mutable-hash-remove! ht k)
  (lock-acquire (mutable-hash-lock ht))
  (let ([cell (and (mutable-hash-cells ht)
                   (hashtable-ref-cell (mutable-hash-ht ht) k))])
    (cond
     [cell
      (hashtable-delete! (mutable-hash-ht ht) k)
      ;; Clear cell, because it may be in `(locked-iterable-hash-cells ht)`
      (set-car! cell #!bwp)
      (set-cdr! cell #!bwp)
      (set-locked-iterable-hash-retry?! ht #t)]
     [else
      (hashtable-delete! (mutable-hash-ht ht) k)]))
  (lock-release (mutable-hash-lock ht)))

(define (hash-clear! ht)
  (cond
   [(mutable-hash? ht)
    (cond
     [(current-future) (future-sync 'hash-clear! (lambda () (mutable-hash-clear! ht)))]
     [else (mutable-hash-clear! ht)])]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (mutable-hash? ht)))
    (unless (impersonate-hash-clear ht #t)
      ;; fall back to iterated remove
      (let loop ([i (hash-iterate-first ht)])
        (when i
          (let ([k (hash-iterate-key ht i none2)])
            (unless (eq? k none2)
              (hash-remove! ht k)))
          (loop (hash-iterate-next ht i)))))]
   [else (raise-argument-error 'hash-clear! "(and/c hash? (not/c immutable?))" ht)]))

(define (mutable-hash-clear! ht)
  (lock-acquire (mutable-hash-lock ht))
  (set-locked-iterable-hash-cells! ht #f)
  (hashtable-clear! (mutable-hash-ht ht))
  (lock-release (mutable-hash-lock ht)))

(define (hash-copy ht)
  (cond
   [(mutable-hash? ht)
    (cond
     [(and (current-future) (eq-mutable-hash? ht))
      (future-sync 'hash-copy (lambda () (mutable-hash-copy ht)))]
     [else (mutable-hash-copy ht)])]
   [(intmap? ht)
    (let ([new-ht (cond
                   [(intmap-eq? ht) (make-hasheq)]
                   [(intmap-eqv? ht) (make-hasheqv)]
                   [(intmap-equal-always? ht) (make-hashalw)]
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

(define (mutable-hash-copy ht)
  (lock-acquire (mutable-hash-lock ht))
  (let ([new-ht (if (eq-mutable-hash? ht)
                    (create-eq-mutable-hash (hashtable-copy (mutable-hash-ht ht) #t))
                    (create-mutable-hash (hashtable-copy (mutable-hash-ht ht) #t)
                                         (cond
                                          [(hash-eqv? ht) 'eqv?]
                                          [(hash-equal-always? ht) 'equal-always?]
                                          [else 'equal?])))])
    (lock-release (mutable-hash-lock ht))
    new-ht))

(define (hash-set ht k v)
  (cond
   [(intmap? ht) (intmap-set ht k v)]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (impersonate-hash-set ht k v)]
   [else (raise-argument-error 'hash-set "(and/c hash? immutable?)" 0 ht k v)]))

(define (hash-remove ht k)
  (cond
   [(intmap? ht) (intmap-remove ht k)]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (impersonate-hash-remove ht k)]
   [else (raise-argument-error 'hash-remove "(and/c hash? immutable?)" 0 ht k)]))

(define (hash-clear ht)
  (cond
   [(intmap? ht)
    (cond
     [(hash-eq? ht) empty-hasheq]
     [(hash-eqv? ht) empty-hasheqv]
     [(hash-equal-always? ht) empty-hashalw]
     [else empty-hash])]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (or (impersonate-hash-clear ht #f)
        ;; fall back to iterated remove
        (let loop ([ht ht])
          (let ([i (hash-iterate-first ht)])
            (if i
                (loop (hash-remove ht (hash-iterate-key ht i)))
                ht))))]
   [else (raise-argument-error 'hash-clear "(and/c hash? immutable?)" ht)]))

(define/who (unsafe-hash-seal! ht)
  (check who eq-mutable-hash? ht)
  (prepare-iterate! ht (hash-count ht))
  (set-locked-iterable-hash-lock! ht #f))

(define/who (hash-eq? ht)
  (cond
   [(mutable-hash? ht) (eq-mutable-hash? ht)]
   [(intmap? ht)
    (intmap-eq? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-eq? (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define/who (hash-eqv? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) eqv?)]
   [(intmap? ht)
    (intmap-eqv? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-eqv? (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define/who (hash-equal? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) key-equal?)]
   [(intmap? ht)
    (intmap-equal? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-equal? (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define/who (hash-equal-always? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) key-equal-always?)]
   [(intmap? ht)
    (intmap-equal-always? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-equal-always? (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define/who (hash-strong? ht)
  (cond
    [(mutable-hash? ht)
     (let ([t (mutable-hash-ht ht)])
       (not (or (hashtable-weak? t)
                (hashtable-ephemeron? t))))]
    [(intmap? ht) #t]
    [(and (impersonator? ht)
          (authentic-hash? (impersonator-val ht)))
     (hash-strong? (impersonator-val ht))]
    [else (raise-argument-error who "hash?" ht)]))

(define/who (hash-weak? ht)
  (cond
   [(mutable-hash? ht)
    (hashtable-weak? (mutable-hash-ht ht))]
   [(intmap? ht) #f]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-weak? (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define/who (hash-ephemeron? ht)
  (cond
   [(mutable-hash? ht)
    (hashtable-ephemeron? (mutable-hash-ht ht))]
   [(intmap? ht) #f]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-ephemeron? (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define/who hash-ref
  (case-lambda
   [(ht k)
    (let ([v (hash-ref/none ht k)])
      (if (eq? v none)
          (raise-arguments-error
           who
           "no value found for key"
           "key" k)
          v))]
   [(ht k fail)
    (let ([v (hash-ref/none ht k)])
      (if (eq? v none)
          (fail-hash-ref who fail)
          v))]))

(define (hash-ref/none ht k)
  (cond
   [(mutable-hash? ht)
    (cond
     [(eq-mutable-hash? ht)
      ;; As long as we'e not in a future thread, it's an atomic action
      ;; to access the mutable hash table using `eq-hashtable-ref`:
      (if (current-future)
          (future-sync 'hash-ref (lambda () (eq-hashtable-ref (mutable-hash-ht ht) k none)))
          (eq-hashtable-ref (mutable-hash-ht ht) k none))]
     [else
      (lock-acquire (mutable-hash-lock ht))
      (let ([v (hashtable-ref (mutable-hash-ht ht) k none)])
        (lock-release (mutable-hash-lock ht))
        v)])]
   [(intmap? ht)
    (intmap-ref ht k none)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-ref ht k)]
   [else
    (raise-argument-error 'hash-ref "hash?" 0 ht k)]))

(define/who hash-ref-key
  (case-lambda
   [(ht k)
    (let ([v (hash-ref-key/none ht k)])
      (if (eq? v none)
          (raise-arguments-error
           who
           "hash does not contain key"
           "key" k)
          v))]
   [(ht k fail)
    (let ([v (hash-ref-key/none ht k)])
      (if (eq? v none)
          (fail-hash-ref who fail)
          v))]))

(define (hash-ref-key/none ht k)
  (cond
   [(mutable-hash? ht)
    (cond
     [(and (current-future) (eq-mutable-hash? ht))
      (future-sync 'hash-ref-key (lambda () (mutable-hash-ref-key/none ht k)))]
     [else
      (mutable-hash-ref-key/none ht k)])]
   [(intmap? ht)
    (intmap-ref-key ht k none)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-ref-key ht k)]
   [else
    (raise-argument-error 'hash-ref-key "hash?" 0 ht k)]))

(define (mutable-hash-ref-key/none ht k)
  (lock-acquire (mutable-hash-lock ht))
  (let* ([pair (hashtable-ref-cell (mutable-hash-ht ht) k)]
         [v (if pair (car pair) none)])
    (lock-release (mutable-hash-lock ht))
    v))

(define (fail-hash-ref who default)
  (if (procedure? default)
      (if (procedure-arity-includes? default 0)
          (|#%app| default)
          (raise (|#%app|
                  exn:fail:contract:arity
                  (error-message->adjusted-string
                   who primitive-realm
                   (string-append
                    "arity mismatch for failure procedure;\n"
                    " given procedure does not accept zero arguments\n"
                    "  procedure: "
                    (error-value->string default))
                   primitive-realm)
                  (current-continuation-marks))))
      default))

(define/who hash-for-each
  (case-lambda
   [(ht proc) (hash-for-each ht proc #f)]
   [(ht proc try-order?)
    (check who hash? ht)
    (check who (procedure-arity-includes/c 2) proc)
    (cond
     [try-order?
      (for-each (lambda (p) (proc (car p) (cdr p)))
                (try-sort-keys (hash-map ht cons)))]
     [(intmap? ht) (intmap-for-each ht proc)]
     [(mutable-hash? ht)
      (mutable-hash-map ht proc #f)]
     [else
      ;; mutable and impersonated:
      (let loop ([i (hash-iterate-first ht)])
        (when i
          (let-values ([(key val) (hash-iterate-key+value ht i none2)])
            (unless (eq? key none2)
              (|#%app| proc key val)))
          (loop (hash-iterate-next ht i))))])]))

(define/who hash-map
  (case-lambda
   [(ht proc) (hash-map ht proc #f)]
   [(ht proc try-order?)
    (check who hash? ht)
    (check who (procedure-arity-includes/c 2) proc)
    (cond
     [try-order?
      (map (lambda (p) (proc (car p) (cdr p)))
           (try-sort-keys (hash-map ht cons)))]
     [(intmap? ht) (intmap-map ht proc)]
     [(mutable-hash? ht)
      (mutable-hash-map ht proc #t)]
     [else
      ;; mutable and impersonated:
      (let loop ([i (hash-iterate-first ht)])
        (if (not i)
            '()
            (cons
             (let-values ([(key val) (hash-iterate-key+value ht i none2)])
               (unless (eq? key none2)
                 (|#%app| proc key val)))
             (loop (hash-iterate-next ht i)))))])]))

(define (mutable-hash-map ht proc map?)
  ;; Inline iteration over the internal vector to avoid the overhead
  ;; of calling `hash-iterate-...` for each step
  (let vec-loop ([old-n 0] [try? #t])
    (let ([vec (prepare-iterate! ht old-n)])
      (cond
        [(fx>= old-n (#%vector-length vec))
         ;; If `old-n` is not zero, the hash table changed while we
         ;; iterated, which is possible since we haven't taken a lock
         (if map? '() (void))]
        [else
         (let loop ([i old-n])
           (cond
             [(fx= i (#%vector-length vec))
              (if try?
                  (vec-loop i (fx> i old-n))
                  (if map? '() (void)))]
             [else
              (let ([p (#%vector-ref vec i)])
                (let ([key (car p)]
                      [val (cdr p)])
                  (cond
                    [(or (eq? key #!bwp)
                         (eq? val #!bwp))
                     (loop (fx+ i 1))]
                    [map?
                     (cons (|#%app| proc key val)
                           (loop (fx+ i 1)))]
                    [else
                     (|#%app| proc key val)
                     (loop (fx+ i 1))])))]))]))))

;; In sorted hash-table travesals, make some effort to sort the key.
;; This attempt is useful for making hash-table traversals more
;; deterministic, especially for marshaling operations.
(define (try-sort-keys ps)
  (cond
   [(#%andmap (lambda (p) (orderable? (car p))) ps)
    (#%list-sort (lambda (a b) (orderable<? (car a) (car b))) ps)]
   [else ps]))

(define (orderable-major v)
  (cond
   [(boolean? v)    0]
   [(char? v)       1]
   [(real? v)       2]
   [(symbol? v)     3]
   [(keyword? v)    4]
   [(string? v)     5]
   [(bytevector? v) 6]
   [(null? v)       7]
   [(void? v)       8]
   [(eof-object? v) 9]
   [else #f]))

(define (orderable? v) (orderable-major v))

(define (orderable<? a b)
  (let ([am (orderable-major a)]
        [bm (orderable-major b)])
    (cond
     [(or (not am) (not bm))
      #f]
     [(fx=? am bm)
      (cond
       [(boolean? a) (not a)]
       [(char? a) (char<? a b)]
       [(real? a) (< a b)]
       [(symbol? a)
        (cond
         [(symbol-interned? a)
          (and (symbol-interned? b)
               (symbol<? a b))]
         [(symbol-interned? b) #t]
         [(symbol-unreadable? a)
          (and (symbol-unreadable? b)
               (symbol<? a b))]
         [(symbol-unreadable? b) #t]
         [else (symbol<? a b)])]
       [(keyword? a) (keyword<? a b)]
       [(string? a) (string<? a b)]
       [(bytevector? a) (bytes<? a b)]
       [else #f])]
     [else (fx<? am bm)])))

(define (hash-count ht)
  (cond
   [(mutable-hash? ht)
    (cond
     [(current-future)
      (future-sync 'hash-count (lambda () (hashtable-size (mutable-hash-ht ht))))]
     [else
      (hashtable-size (mutable-hash-ht ht))])]
   [(intmap? ht) (intmap-count ht)]
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
                  (intmap-equal? ht2))
             (and (intmap-equal-always? ht1)
                  (intmap-equal-always? ht2))))
    (intmap-keys-subset? ht1 ht2)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2))
             (and (hash-equal-always? ht1)
                  (hash-equal-always? ht2))))
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
                           "second table" ht2)]))

;; Use `eql?` for recursive comparisons
(define (hash=? ht1 ht2 eql?)
  (cond
   [(and (intmap? ht1)
         (intmap? ht2))
    (intmap=? ht1 ht2 eql?)]
   [(and (hash? ht1)
         (hash? ht2)
         ;; Same mutability?
         (eq? (intmap? (strip-impersonator ht1))
              (intmap? (strip-impersonator ht2)))
         ;; Same key comparison?
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2))
             (and (hash-equal-always? ht1)
                  (hash-equal-always? ht2)))
         ;; Same weakness?
         (eq? (hash-weak? ht1) (hash-weak? ht2))
         (eq? (hash-ephemeron? ht1) (hash-ephemeron? ht2)))
    (and (= (hash-count ht1) (hash-count ht2))
         ;; This generic comparison supports impersonators
         (let loop ([i (hash-iterate-first ht1)])
           (cond
            [(not i) #t]
            [else
             (let-values ([(key val) (hash-iterate-key+value ht1 i none2)])
               (if (eq? key none2)
                   ;; Ill-timed GC => start over
                   (hash=? ht1 ht2 eql?)
                   (let ([val2 (hash-ref ht2 key none)])
                     (cond
                      [(eq? val2 none) #f]
                      [else (and (eql? val val2)
                                 (loop (hash-iterate-next ht1 i)))]))))])))]
   [else #f]))


;; Use `hash` for recursive hashing on values
(define (hash-hash-code ht hash)
  (let* ([eq-key? (hash-eq? ht)]
         [eqv-key? (and (not eq-key?) (hash-eqv? ht))]
         [equal-always-key? (and (not eq-key?) (not eqv-key?) (hash-equal-always? ht))])
    (cond
      [(intmap? ht) 
       (hash-code-combine
        (intmap-hash-code ht hash)
        (cond [eq-key? 0] [eqv-key? 1] [equal-always-key? 2] [else 3]))]
      [else
       ;; This generic hashing supports impersonators
       (let loop ([hc 0] [i (hash-iterate-first ht)])
         (cond
           [(not i)
            (hash-code-combine
             (hash-code-combine
              hc
              (cond [eq-key? 0] [eqv-key? 1] [equal-always-key? 2] [else 3]))
             (cond [(intmap? ht) 0] [(hash-weak? ht) 1] [(hash-ephemeron? ht) 2] [else 3]))]
           [else
            (let-values ([(key val) (hash-iterate-key+value ht i none2)])
              (if (eq? key none2)
                  (loop hc (hash-iterate-next ht i))
                  (let ([hk (cond
                              [eq-key? (eq-hash-code key)]
                              [eqv-key? (eqv-hash-code key)]
                              [equal-always-key? (equal-always-hash-code key)]
                              [else (equal-hash-code key)])])
                    (loop (hash-code-combine-unordered hc (hash-code-combine hk (hash val)))
                          (hash-iterate-next ht i)))))]))])))


;; Start by getting just a few cells via `hashtable-cells`,
;; and then get more as needed, so that an N-step traversals
;; is O(N) even if the hash table has more than O(N) entries.
(define (prepare-iterate! ht i)
  (lock-acquire (locked-iterable-hash-lock ht))
  (let ([vec (locked-iterable-hash-cells ht)])
    (cond
     [(and vec
           (fixnum? i)
           (let ([len (#%vector-length vec)])
             (or (fx> len i)
                 (and (fx= len i)
                      (not (locked-iterable-hash-retry? ht))))))
      (lock-release (locked-iterable-hash-lock ht))
      vec]
     [else
      (let ([new-vec (get-locked-iterable-hash-cells
                      ht
                      (fxmax (if vec
                                 (fx* 2 (#%vector-length vec))
                                 0)
                             32))])
        (let ([len (#%vector-length new-vec)])
          (when (fx= len (hash-count ht))
            (set-locked-iterable-hash-retry?! ht #f)))
        (let ([vec (cells-merge vec new-vec)])
          (set-locked-iterable-hash-cells! ht vec)
          (lock-release (locked-iterable-hash-lock ht))
          vec))])))

(define (get-locked-iterable-hash-cells ht n)
  (hashtable-cells (mutable-hash-ht ht) n))

;; Separate calls to `hashtable-cells` may return the
;; cells in a different order, so we have to merge the
;; tables. The resulting vector starts with the same
;; elements as `vec`.
(define (cells-merge vec new-vec)
  (cond
   [(not vec)
    ;; Nothing to merge
    new-vec]
   ;; Common case: same order
   [(let ([len (#%vector-length vec)])
      (and (fx= len (#%vector-length new-vec))
           (let loop ([i 0])
             (or (fx= i len)
                 (and (eq? (#%vector-ref vec i) (#%vector-ref new-vec i))
                      (loop (fx+ i 1)))))))
    new-vec]
   [else
    ;; General case
    (let ([new-ht (make-eq-hashtable)])
      (vector-for-each (lambda (p) (hashtable-set! new-ht p #t)) new-vec)
      (vector-for-each (lambda (p) (hashtable-delete! new-ht p)) vec)
      (let ([merge-vec (#%make-vector (fx+ (#%vector-length vec) (hashtable-size new-ht)))])
        (let loop ([i (#%vector-length vec)])
          (unless (fx= i 0)
            (let ([i (fx- i 1)])
              (#%vector-set! merge-vec i (#%vector-ref vec i))
              (loop i))))
        (let ([new-len (#%vector-length new-vec)])
          (let loop ([i 0] [j (#%vector-length vec)])
            (unless (fx= i new-len)
              (let ([p (#%vector-ref new-vec i)])
                (cond
                 [(hashtable-contains? new-ht p)
                  (#%vector-set! merge-vec j p)
                  (loop (fx+ i 1) (fx+ j 1))]
                 [else
                  (loop (fx+ i 1) j)])))))
        merge-vec))]))

(define/who (hash-iterate-first ht)
  (cond
   [(intmap? ht)
    (intmap-iterate-first ht)]
   [(locked-iterable-hash? ht)
    (locked-iterable-hash-iterate-next ht #f)]
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
   [(locked-iterable-hash? ht)
    (check-i 'hash-iterate-next i)
    (locked-iterable-hash-iterate-next ht i)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    ;; `hash-iterate-next` must not hash any keys:
    (hash-iterate-next (impersonator-val ht) i)]
   [else (raise-argument-error who "hash?" 0 ht i)]))

(define (locked-iterable-hash-iterate-next ht init-i)
  (let loop ([i (or init-i -1)])
    (let* ([i (add1 i)]
           [vec (prepare-iterate! ht i)] ; vec expected to have >= `i` elements
           [len (#%vector-length vec)])
      (cond
       [(> i len)
        #f
        #;
        (raise-arguments-error 'hash-iterate-next "no element at index"
                               "index" init-i
                               "within length" len
                               "vec" vec)]
       [(= i len)
        #f]
       [else
        (let* ([p (#%vector-ref vec i)]
               [key (car p)])
          (cond
           [(bwp-object? key)
            ;; A hash table change or disappeared weak reference
            (loop i)]
           [else i]))]))))

(define (do-hash-iterate-key+value who ht i
                                   intmap-iterate-key+value
                                   key? value? pair?
                                   bad-index-v)
  (cond
   [(intmap? ht)
    (check-i who i)
    (call-with-values (lambda () (intmap-iterate-key+value ht i none))
      (case-lambda
        [(v) (if (eq? v none)
                 (if (eq? bad-index-v none)
                     (raise-arguments-error who "no element at index"
                                            "index" i)
                     (bad-index-result key? value? pair? bad-index-v))
                 v)]
        [(k v) (values k v)]))]
   [(locked-iterable-hash? ht)
    (check-i who i)
    (let* ([vec (prepare-iterate! ht i)]
           [len (#%vector-length vec)]
           [p (if (fx< i len)
                  (#%vector-ref vec i)
                  '(#!bwp . #!bwp))]
           [key (car p)]
           [v (cdr p)])
      (if (or (bwp-object? key)
              (bwp-object? v))
          (if (eq? bad-index-v none)
              (raise-arguments-error who "no element at index"
                                     "index" i)
              (bad-index-result key? value? pair? bad-index-v))
          (cond
           [key?
            (if value?
                (if pair?
                    (cons key v)
                    (values key v))
                key)]
           [else v])))]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-iterate-key+value who ht i key? value? pair? bad-index-v)]
   [else (raise-argument-error who "hash?" ht)]))

(define hash-iterate-key
  (case-lambda
   [(ht i) (hash-iterate-key ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-key ht i
                               intmap-iterate-key
                               #t #f #f
                               bad-index-v)]))

(define hash-iterate-value
  (case-lambda
   [(ht i) (hash-iterate-value ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-value ht i
                               intmap-iterate-value
                               #f #t #f
                               bad-index-v)]))

(define hash-iterate-key+value
  (case-lambda
   [(ht i) (hash-iterate-key+value ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-key+value ht i
                               intmap-iterate-key+value
                               #t #t #f
                               bad-index-v)]))

(define hash-iterate-pair
  (case-lambda
   [(ht i) (hash-iterate-pair ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-pair ht i
                               intmap-iterate-pair
                               #t #t #t
                               bad-index-v)]))

(define (unsafe-immutable-hash-iterate-first ht)
  (if (impersonator? ht)
      (hash-iterate-first ht)
      (unsafe-intmap-iterate-first ht)))

(define (iterator-for-impersonator? i) (fixnum? i))

(define (unsafe-immutable-hash-iterate-next ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-next ht i)
      (unsafe-intmap-iterate-next ht i)))

(define unsafe-immutable-hash-iterate-key
  (case-lambda
   [(ht i)
    (unsafe-immutable-hash-iterate-key ht i none)]
   [(ht i bad-index-v)
    (if (iterator-for-impersonator? i)
        (hash-iterate-key ht i)
        (unsafe-intmap-iterate-key ht i))]))

(define unsafe-immutable-hash-iterate-value
  (case-lambda
   [(ht i) (unsafe-immutable-hash-iterate-value ht i none)]
   [(ht i bad-index-v)
    (if (iterator-for-impersonator? i)
        (hash-iterate-value ht i bad-index-v)
        (unsafe-intmap-iterate-value ht i))]))

(define unsafe-immutable-hash-iterate-key+value
  (case-lambda
   [(ht i) (unsafe-immutable-hash-iterate-key+value ht i none)]
   [(ht i bad-index-v)
    (if (iterator-for-impersonator? i)
        (hash-iterate-key+value ht i bad-index-v)
        (unsafe-intmap-iterate-key+value ht i))]))

(define unsafe-immutable-hash-iterate-pair
  (case-lambda
   [(ht i) (unsafe-immutable-hash-iterate-pair ht i none)]
   [(ht i bad-index-v) 
    (if (iterator-for-impersonator? i)
        (hash-iterate-pair ht i bad-index-v)
        (unsafe-intmap-iterate-pair ht i))]))

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

(define unsafe-ephemeron-hash-iterate-first hash-iterate-first)
(define unsafe-ephemeron-hash-iterate-next hash-iterate-next)
(define unsafe-ephemeron-hash-iterate-key hash-iterate-key)
(define unsafe-ephemeron-hash-iterate-value hash-iterate-value)
(define unsafe-ephemeron-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-ephemeron-hash-iterate-pair hash-iterate-pair)

;; ----------------------------------------

(define (set-hash-hash!)
  (struct-set-equal+hash! (record-type-descriptor mutable-hash)
                          hash=?
                          hash-hash-code)
  (struct-set-equal+hash! (record-type-descriptor hash-impersonator)
                          #f
                          hash-hash-code)
  (struct-set-equal+hash! (record-type-descriptor hash-chaperone)
                          #f
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
                       (mutable-hash? p)))
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
                                (procedure? (car args))))]
         [clear (if clear-given?
                    (let ([clear (car args)])
                      (check who (procedure-arity-includes/c 1) :or-false clear)
                      clear)
                    void)]
         [args (if clear-given? (cdr args) args)]
         [equal-key-given? (and (pair? args)
                                (or (not (car args))
                                    (procedure? (car args))))]
         [equal-key (if equal-key-given?
                        (let ([equal-key (car args)])
                          (check who (procedure-arity-includes/c 2) :or-false equal-key)
                          equal-key)
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
  (impersonate-hash-ref/set 'hash-ref "value" #f
                            (lambda (ht k v) (hash-ref ht k none))
                            (lambda (procs ht k none-v)
                              (|#%app| (hash-procs-ref procs) ht k))
                            hash-procs-ref
                            ht k none))

(define (impersonate-hash-ref-key ht k)
  (impersonate-hash-ref/set 'hash-ref-key "key" #f
                            (lambda (ht k v) (hash-ref-key ht k none))
                            (lambda (procs ht k none-v)
                              (let-values ([(new-k _) (|#%app| (hash-procs-ref procs) ht k)])
                                (values new-k
                                        (lambda (ht given-k actual-k)
                                          (|#%app| (hash-procs-key procs) ht actual-k)))))
                            hash-procs-ref
                            ht k none))

(define (impersonate-hash-set! ht k v)
  (impersonate-hash-ref/set 'hash-set! "void" #t
                            hash-set!
                            (lambda (procs ht k v)
                              (|#%app| (hash-procs-set procs) ht k v))
                            hash-procs-set
                            ht k v))

(define (impersonate-hash-set ht k v)
  (impersonate-hash-ref/set 'hash-set "hash" #t
                            hash-set
                            (lambda (procs ht k v)
                              (|#%app| (hash-procs-set procs) ht k v))
                            hash-procs-set
                            ht k v))

(define (impersonate-hash-remove! ht k)
  (impersonate-hash-ref/set 'hash-remove! "void" #t
                            (lambda (ht k false-v) (hash-remove! ht k))
                            (lambda (procs ht k false-v)
                              (values (|#%app| (hash-procs-remove procs) ht k) #f))
                            hash-procs-remove
                            ht k #f))

(define (impersonate-hash-remove ht k)
  (impersonate-hash-ref/set 'hash-remove "hash" #t
                            (lambda (ht k false-v) (hash-remove ht k))
                            (lambda (procs ht k false-v)
                              (values (|#%app| (hash-procs-remove procs) ht k) #f))
                            hash-procs-remove
                            ht k #f))

(define (impersonate-hash-ref/set who what-r set? authentic-op apply-wrapper get-wrapper ht k v)
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
                    (raise-chaperone-error who "key" k new-k))
                  (when set?
                    (unless (or (not chaperone?) (chaperone-of? new-v-or-wrap v))
                      (raise-chaperone-error who "value" v new-v-or-wrap))))
                ;; Recur...
                (let ([r (loop next-ht get-k new-k (if set? new-v-or-wrap none))])
                  ;; In `ref` mode, `r` is the result value (hash-ref) or key (hash-ref-key).
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
                          (raise-chaperone-error who what-r r new-r)))
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
           [new-k (|#%app| (hash-procs-equal-key procs) next-ht k)])
      (unless (or (not chaperone?) (chaperone-of? new-k k))
        (raise-chaperone-error who "key" k new-k))
      new-k)))

(define (impersonate-hash-clear ht mutable?)
  (let loop ([ht ht])
    (cond
     [(or (hash-impersonator? ht)
          (hash-chaperone? ht))
      (let ([procs (if (hash-impersonator? ht)
                       (hash-impersonator-procs ht)
                       (hash-chaperone-procs ht))]
            [next-ht (impersonator-next ht)])
        (let ([clear (hash-procs-clear procs)])
          (cond
           [clear
            (|#%app| clear next-ht)
            (if mutable?
                (loop next-ht)
                (let ([r (loop next-ht)])
                  (and r
                       ((if (chaperone? ht) make-hash-chaperone make-hash-impersonator)
                        (strip-impersonator r)
                        r
                        (impersonator-props ht)
                        procs))))]
           [else
            ;; Fall back to iterate of remove
            #f])))]
     [(impersonator? ht)
      (if mutable?
          (loop (impersonator-next ht))
          (let ([r (loop (impersonator-next ht))])
            (and r
                 (rewrap-props-impersonator ht r))))]
     [else
      (if mutable?
          (hash-clear! ht)
          (hash-clear ht))])))

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
               [(hash-equal-always? val-ht) (make-weak-hashalw)]
               [else (make-weak-hash)])]
             [(hash-ephemeron? ht)
              (cond
               [(hash-eq? val-ht) (make-ephemeron-hasheq)]
               [(hash-eqv? val-ht) (make-ephemeron-hasheq)]
               [(hash-equal-always? val-ht) (make-ephemeron-hashalw)]
               [else (make-ephemeron-hash)])]
             [else
              (cond
               [(hash-eq? val-ht) (make-hasheq)]
               [(hash-eqv? val-ht) (make-hasheq)]
               [(hash-equal-always? val-ht) (make-hashalw)]
               [else (make-hash)])])]
           [else
            (cond
             [(hash-eq? val-ht) (make-hasheq)]
             [(hash-eqv? val-ht) (make-hasheqv)]
             [(hash-equal-always? val-ht) (make-hashalw)]
             [else (make-hash)])])])
    (let loop ([i (hash-iterate-first ht)])
      (cond
       [i (let-values ([(key val) (hash-iterate-key+value ht i none2)])
            (unless (eq? key none2)
              (hash-set! new-ht key val))
            (loop (hash-iterate-next ht i)))]
       [else new-ht]))))

(define (impersonate-hash-iterate-key+value who ht i key? value? pair? bad-index-v)
  (let ([key (impersonate-hash-iterate-key who ht i (if (eq? bad-index-v none) none none2))])
    (cond
     [(eq? key none2) (bad-index-result key? value? pair? bad-index-v)]
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

(define (impersonate-hash-iterate-key who ht i bad-index-v)
  ;; We don't have to set up `get-k`, because `hash-iterate-key`
  ;; is prohibited from hashing any keys
  (let loop ([ht ht])
    (cond
     [(hash-impersonator? ht)
      (let ([procs (hash-impersonator-procs ht)]
            [ht (impersonator-next ht)])
        (|#%app| (hash-procs-key procs) ht (loop ht)))]
     [(hash-chaperone? ht)
      (let ([procs (hash-chaperone-procs ht)]
            [ht (impersonator-next ht)])
        (let* ([k (loop ht)]
               [new-k (|#%app| (hash-procs-key procs) ht k)])
          (unless (chaperone-of? new-k k)
            (raise-chaperone-error who "key" k new-k))
          new-k))]
     [(impersonator? ht)
      (loop (impersonator-next ht))]
     [else
      ;; The same as `hash-iterate-key`, but with the correct `who`:
      (do-hash-iterate-key+value who ht i
                                 intmap-iterate-key
                                 #t #f #f
                                 bad-index-v)])))

(define (bad-index-result key? value? pair? bad-index-v)
  (cond
   [pair? (cons bad-index-v bad-index-v)]
   [(and value? key?) (values bad-index-v bad-index-v)]
   [else bad-index-v]))
