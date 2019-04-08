;; Mutable and weak-equal hash tables need a lock
;; and an iteration vector
(define-record locked-iterable-hash (lock
                                     cells    ; vector of cells for iteration
                                     retry?)) ; is `cells` maybe incomplete?

;; To support iteration and locking, we wrap Chez's mutable hash
;; tables in a `mutable-hash` record
(define-record mutable-hash locked-iterable-hash
  (ht)) ; Chez Scheme hashtable
(define (create-mutable-hash ht kind) (make-mutable-hash (make-lock kind) #f #f ht))

(define (mutable-hash-lock ht) (locked-iterable-hash-lock ht))
(define (mutable-hash-cells ht) (locked-iterable-hash-cells ht))

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

(define (eq-hashtable->hash ht)
  (create-mutable-hash ht 'eq?))
(define (hash->eq-hashtable ht)
  (mutable-hash-ht ht))

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
       (define vararg-ctor
         (case-lambda
          [() empty-hash]
          [kvs
           (let loop ([kvs kvs] [h empty-hash])
             (cond
              [(null? kvs) h]
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
(define-hash-constructors hasheqv make-immutable-hasheqv empty-hasheqv)
(define-hash-constructors hasheq make-immutable-hasheq empty-hasheq)

(define (hash-set! ht k v)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (hashtable-set! (mutable-hash-ht ht) k v)
    (set-locked-iterable-hash-retry?! ht #t)
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
    (cond
     [(and (mutable-hash-cells ht)
           (hashtable-contains? (mutable-hash-ht ht) k))
      (let ([cell (hashtable-cell (mutable-hash-ht ht) k #f)])
        (hashtable-delete! (mutable-hash-ht ht) k)
        ;; Clear cell, because it may be in `(locked-iterable-hash-cells ht)`
        (set-car! cell #!bwp)
        (set-cdr! cell #!bwp)
        (set-locked-iterable-hash-retry?! ht #t))]
     [else
      (hashtable-delete! (mutable-hash-ht ht) k)])
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
    (set-locked-iterable-hash-cells! ht #f)
    (hashtable-clear! (mutable-hash-ht ht))
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-clear! ht)]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (or (mutable-hash? ht)
               (weak-equal-hash? ht))))
    (unless (impersonate-hash-clear ht #t)
      ;; fall back to iterated remove
      (let loop ([i (hash-iterate-first ht)])
          (when i
            (hash-remove! ht (hash-iterate-key ht i))
            (loop (hash-iterate-next ht i)))))]
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
   [else (raise-argument-error 'hash-set "(and/c hash? immutable?)" ht)]))

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
    (or (impersonate-hash-clear ht #f)
        ;; fall back to iterated remove
        (let loop ([ht ht])
          (let ([i (hash-iterate-first ht)])
            (if i
                (loop (hash-remove ht (hash-iterate-key ht i)))
                ht))))]
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
             ($fail fail)
             v))]
      [(intmap? ht) (intmap-ref ht k fail)]
      [(weak-equal-hash? ht) (weak-hash-ref ht k fail)]
      [(and (impersonator? ht)
            (authentic-hash? (impersonator-val ht)))
       (let ([v (impersonate-hash-ref ht k)])
         (if (eq? v none)
             ($fail fail)
             v))]
      [else (raise-argument-error 'hash-ref "hash?" ht)])]))

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
     [else
      ;; mutable, impersonated, and weak-equal:
      (let loop ([i (hash-iterate-first ht)])
        (when i
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (|#%app| proc key val))
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
     [else
      ;; mutable, impersonated, and weak-equal:
      (let loop ([i (hash-iterate-first ht)])
        (if (not i)
            '()
            (cons
             (let-values ([(key val) (hash-iterate-key+value ht i)])
               (|#%app| proc key val))
             (loop (hash-iterate-next ht i)))))])]))

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
      (let ([weak? (locked-iterable-hash-weak? ht)]
            [new-vec (get-locked-iterable-hash-cells
                      ht
                      (fxmax (if vec
                                 (fx* 2 (#%vector-length vec))
                                 0)
                             32))])
        (let ([len (#%vector-length new-vec)])
          (when (fx= len (hash-count ht))
            (set-locked-iterable-hash-retry?! ht #f))
          (when weak?
            (let loop ([i 0])
              (unless (fx= i len)
                (let ([p (#%vector-ref new-vec i)])
                  (#%vector-set! new-vec i (ephemeron/fl-cons (car p) p)))
                (loop (fx+ i 1))))))
        (let ([vec (cells-merge vec new-vec weak?)])
          (set-locked-iterable-hash-cells! ht vec)
          (lock-release (locked-iterable-hash-lock ht))
          vec))])))

(define (get-locked-iterable-hash-cells ht n)
  (cond
   [(mutable-hash? ht) (hashtable-cells (mutable-hash-ht ht) n)]
   [else (weak-equal-hash-cells ht n)]))

(define (locked-iterable-hash-weak? ht)
  (cond
    [(mutable-hash? ht) (hashtable-weak? (mutable-hash-ht ht))]
    [else #t]))

;; Separate calls to `hashtable-cells` may return the
;; cells in a different order, so we have to merge the
;; tables. The resulting vector starts with the same
;; elements as `vec`.
(define (cells-merge vec new-vec weak?)
  (cond
   [(not vec)
    ;; Nothing to merge
    new-vec]
   ;; Common case: same order
   [(let ([len (#%vector-length vec)])
      (and (fx= len (#%vector-length new-vec))
           (let loop ([i 0])
             (or (fx= i len)
                 (and (if weak?
                          (eq? (cdr (#%vector-ref vec i)) (cdr (#%vector-ref new-vec i)))
                          (eq? (#%vector-ref vec i) (#%vector-ref new-vec i)))
                      (loop (fx+ i 1)))))))
    new-vec]
   [else
    ;; General case
    (let ([new-ht (make-eq-hashtable)])
      (vector-for-each (lambda (p) (hashtable-set! new-ht (if weak? (cdr p) p) #t)) new-vec)
      (vector-for-each (lambda (p) (hashtable-delete! new-ht (if weak? (cdr p) p))) vec)
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
                 [(hashtable-contains? new-ht (if weak? (cdr p) p))
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
   [else (raise-argument-error who "hash?" ht)]))

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
        (let* ([p (let ([p (#%vector-ref vec i)])
                    (if (locked-iterable-hash-weak? ht)
                        (let ([p (cdr p)])
                          (if (bwp-object? p)
                              '(#!bwp . #!bwp)
                              p))
                        p))]
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
                  (let ([p (#%vector-ref vec i)])
                    (if (locked-iterable-hash-weak? ht)
                        (let ([p (cdr p)])
                          (if (bwp-object? p)
                              '(#!bwp . #!bwp)
                              p))
                        p))
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

(define (unsafe-immutable-hash-iterate-key ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-key ht i)
      (unsafe-intmap-iterate-key ht i)))

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

;;  ----------------------------------------

;; Chez Scheme doesn't provide weak hash table with `equal?` comparisons,
;; so build our own

(define-record weak-equal-hash locked-iterable-hash
  (keys-ht    ; integer[equal hash code] -> weak list of keys
   vals-ht    ; weak, eq?-based hash table: key -> value
   count      ; number of items in the table (= sum of list lengths)
   prune-at)) ; count at which we should try to prune empty weak boxes

(define (weak-equal-hash-lock t) (locked-iterable-hash-lock t))

(define (make-weak-hash-with-lock lock)
  (make-weak-equal-hash lock #f #f (hasheqv) (make-weak-eq-hashtable) 0 128))

(define make-weak-hash
  (case-lambda
   [() (make-weak-hash-with-lock (make-lock 'equal?))]
   [(alist) (fill-hash! 'make-weak-hash (make-weak-hash) alist)]))

(define (weak-hash-copy ht)
  (lock-acquire (weak-equal-hash-lock ht))
  (let ([new-ht (make-weak-equal-hash (weak-equal-hash-lock ht)
                                      #f
                                      #t
                                      (weak-equal-hash-keys-ht ht)
                                      (hashtable-copy (weak-equal-hash-vals-ht ht) #t)
                                      (weak-equal-hash-count ht)
                                      (weak-equal-hash-prune-at ht))])
    (lock-release (weak-equal-hash-lock ht))
    new-ht))

(define weak-hash-ref
  (case-lambda
   [(t key fail code key-equal?)
    (lock-acquire (weak-equal-hash-lock t))
    (let ([keys (intmap-ref (weak-equal-hash-keys-ht t) code '())])
      (let loop ([keys keys])
        (cond
         [(null? keys)
          ;; Not in the table:
          (lock-release (weak-equal-hash-lock t))
          ($fail fail)]
         [(key-equal? (car keys) key)
          (let* ([k (car keys)]
                 [v (hashtable-ref (weak-equal-hash-vals-ht t) (car keys) none)])
            (lock-release (weak-equal-hash-lock t))
            (if (eq? v none)
                ($fail fail)
                v))]
         [else (loop (cdr keys))])))]
   [(t key fail)
    (weak-hash-ref t key fail (key-equal-hash-code key) key-equal?)]))

;; Only used in atomic mode:
(define (weak-hash-ref-key ht key)
  (let* ([code (key-equal-hash-code key)]
         [keys (intmap-ref (weak-equal-hash-keys-ht ht) code '())])
    (let loop ([keys keys])
      (cond
       [(null? keys) #f]
       [(key-equal? (car keys) key) (car keys)]
       [else (loop (cdr keys))]))))

(define weak-hash-set!
  (case-lambda
   [(t k v code key-equal?)
    (lock-acquire (weak-equal-hash-lock t))
    (set-locked-iterable-hash-retry?! t #t)
    (let ([keys (intmap-ref (weak-equal-hash-keys-ht t) code '())])
      (let loop ([keys keys])
        (cond
         [(null? keys)
          ;; Not in the table:
          (when (= (weak-equal-hash-count t) (weak-equal-hash-prune-at t))
            (prune-table! t))
          (let* ([ht (weak-equal-hash-keys-ht t)])
            (set-weak-equal-hash-count! t
                                        (add1 (weak-equal-hash-count t)))
            (set-weak-equal-hash-keys-ht! t
                                          (intmap-set ht code
                                                      (weak/fl-cons k
                                                                    (intmap-ref ht code '()))))
            (hashtable-set! (weak-equal-hash-vals-ht t) k v))
          (lock-release (weak-equal-hash-lock t))]
         [(key-equal? (car keys) k)
          (let ([k (car keys)])
            (hashtable-set! (weak-equal-hash-vals-ht t) k v))
          (lock-release (weak-equal-hash-lock t))]
         [else (loop (cdr keys))])))]
   [(t k v)
    (weak-hash-set! t k v (key-equal-hash-code k) key-equal?)]))

(define (weak-hash-remove! t k)
  (let ([code (key-equal-hash-code k)])
    (lock-acquire (weak-equal-hash-lock t))
    (let* ([keys (intmap-ref (weak-equal-hash-keys-ht t) code '())]
           [new-keys
            (let loop ([keys keys])
              (cond
               [(null? keys)
                ;; Not in the table
                #f]
               [(let ([a (car keys)])
                  (and (key-equal? a k)
                       a))
                => (lambda (a)
                     (let ([ht (weak-equal-hash-vals-ht t)])
                       (cond
                        [(locked-iterable-hash-cells t)
                         ;; Clear cell, because it may be in `(locked-iterable-hash-cells ht)`
                         (let ([cell (hashtable-cell ht a #f)])
                           (hashtable-delete! ht a)
                           (set-car! cell #!bwp)
                           (set-cdr! cell #!bwp))]
                        [else
                         (hashtable-delete! ht a)]))
                     (cdr keys))]
               [else
                (let ([new-keys (loop (cdr keys))])
                  (and new-keys
                       (let ([a (car keys)])
                         (if (bwp-object? a)
                             new-keys
                             (weak/fl-cons a new-keys)))))]))])
      (when new-keys
        (set-weak-equal-hash-keys-ht! t
                                      (if (null? new-keys)
                                          (intmap-remove (weak-equal-hash-keys-ht t) code)
                                          (intmap-set (weak-equal-hash-keys-ht t) code new-keys))))
      (set-locked-iterable-hash-retry?! t #t)
      (lock-release (weak-equal-hash-lock t)))))

(define (weak-hash-clear! t)
  (lock-acquire (weak-equal-hash-lock t))
  (set-weak-equal-hash-keys-ht! t (hasheqv))
  (hashtable-clear! (weak-equal-hash-vals-ht t))
  (set-weak-equal-hash-count! t 0)
  (set-weak-equal-hash-prune-at! t 128)
  (set-locked-iterable-hash-cells! t #f)
  (lock-release (weak-equal-hash-lock t)))

(define (weak-hash-count t)
  (hashtable-size (weak-equal-hash-vals-ht t)))

(define (weak-equal-hash-cells ht len)
  (let ([vec (#%make-vector len #f)]
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
              (let ([key (car l)])
                (cond
                 [(eq? #!bwp key) (loop (cdr l))]
                 [else
                  (#%vector-set! vec (unbox pos) (hashtable-cell (weak-equal-hash-vals-ht ht) key #f))
                  (set-box! pos (add1 (unbox pos)))
                  (if (= (unbox pos) len)
                      ;; That's enough keys
                      (esc (void))
                      (loop (cdr l)))]))]))))))
    ;; Resize `vec` if we got fewer than `len` keys
    (cond
     [(< (unbox pos) len)
      (let* ([len (unbox pos)]
             [compact-vec (#%make-vector len)])
        (let loop ([i 0])
          (unless (fx= i len)
            (#%vector-set! compact-vec i (#%vector-ref vec i))
            (loop (fx+ i 1))))
        compact-vec)]
     [else vec])))

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

(define (ephemeron/fl-cons key d)
  (if (flonum? key)
      (cons key d)
      (ephemeron-cons key d)))

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
            (clear next-ht)
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
                                 #t #f #f
                                 bad-index-v)])))

(define (bad-index-result key? value? pair? bad-index-v)
  (cond
   [pair? (cons bad-index-v bad-index-v)]
   [(and value? key?) (values bad-index-v bad-index-v)]
   [else bad-index-v]))
