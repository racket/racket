(define (prefab-struct-key v)
  (let ([v (strip-impersonator v)])
    (and (record? v)
         (let ([p (with-global-lock*
                   (getprop (record-type-uid (record-rtd v)) 'prefab-key+count #f))])
           (and p (car p))))))

(define/who (prefab-key->struct-type key field-count)
  (prefab-key+count->rtd
   (cons (normalized-prefab-key/check who key field-count)
         field-count)))

(define/who (make-prefab-struct key . args)
  (let* ([field-count (length args)]
         [norm-key (normalized-prefab-key/check who key field-count)])
    (let ([rtd (prefab-key->struct-type key field-count)])
      (apply (record-constructor rtd) args))))

;; ----------------------------------------

;; Check that `k` is valid as a prefab key
(define (prefab-key? k)
  (or (symbol? k)
      (and (pair? k)
           (symbol? (car k))
           (let* ([k (cdr k)] ; skip name
                  [prev-k k]
                  ;; The initial field count can be omitted:
                  [k (if (and (pair? k)
                              (exact-nonnegative-integer? (car k)))
                         (cdr k)
                         k)]
                  [field-count (if (eq? prev-k k)
                                   #f
                                   (car prev-k))])
             (let loop ([field-count field-count] [k k]) ; `k` is after name and field count
               (or (null? k)
                   (and (pair? k)
                        (let* ([prev-k k]
                               [k (if (and (pair? (car k))
                                           (pair? (cdar k))
                                           (null? (cddar k))
                                           (exact-nonnegative-integer? (caar k)))
                                      ;; Has a (list <auto-count> <auto-val>) element
                                      (cdr k)
                                      ;; Doesn't have auto-value element:
                                      k)]
                               [auto-count (if (eq? prev-k k)
                                               0
                                               (caar prev-k))])
                          (or (null? k)
                              (and (pair? k)
                                   (let* ([k (if (and (pair? k)
                                                      (vector? (car k)))
                                                 ;; Make sure it's a vector of indices
                                                 ;; that are in range and distinct:
                                                 (let* ([vec (car k)]
                                                        [len (vector-length vec)])
                                                   (let loop ([i 0] [set 0])
                                                     (cond
                                                      [(= i len) (cdr k)]
                                                      [else
                                                       (let ([pos (vector-ref vec i)])
                                                         (and (exact-nonnegative-integer? pos)
                                                              (or (not field-count)
                                                                  (< pos (+ field-count auto-count)))
                                                              (not (bitwise-bit-set? set pos))
                                                              (loop (add1 i) (bitwise-ior set (bitwise-arithmetic-shift-left 1 pos)))))])))
                                                 k)])
                                     (or (null? k)
                                         (and (pair? k)
                                              ;; Supertype: make sure it's a pair with a
                                              ;; symbol and a field count, and loop:
                                              (symbol? (car k))
                                              (pair? (cdr k))
                                              (exact-nonnegative-integer? (cadr k))
                                              (loop (cadr k) (cddr k)))))))))))))))

;; Assuming `(prefab-key? k)`, check that it's consistent with the
;; given total field count
(define (prefab-key-compatible-count? k total-field-count)
  (letrec ([field-count-after-name+count
            (lambda (k)
              (cond
               [(null? k) 0]
               [(pair? (car k))
                (+ (caar k)
                   (field-count-after-name+count+auto (cdr k)))]
               [else
                (field-count-after-name+count+auto k)]))]
           [field-count-after-name+count+auto
            (lambda (k)
              (cond
               [(null? k) 0]
               [(vector? (car k))
                (if (null? (cdr k))
                    0
                    (field-count (cdr k)))]
               [else (field-count k)]))]
           [field-count
            (lambda (k) ; k has symbol and count
              (+ (cadr k)
                 (field-count-after-name+count (cddr k))))])
    (cond
     [(symbol? k) #t]
     [(null? (cdr k)) #t]
     [(exact-integer? (cadr k))
      ;; Info must match exactly
      (= total-field-count
         (+ (cadr k) (field-count-after-name+count (cddr k))))]
     [else
      (let ([n (field-count-after-name+count (cdr k))])
        (and
         ;; Field count must be <= total-field-count
         (>= total-field-count n)
         ;; Initial mutables vector (if any) must be in range
         ;; for the target field count; any later immutables vector
         ;; has been checked already by `prefab-key?`
         (let* ([k (cdr k)]
                [auto (and (pair? (car k))
                           (car k))]
                [k (if auto
                       (cdr k)
                       k)])
           (or (null? k)
               (not (vector? (car k)))
               (let* ([n (- total-field-count
                            (if auto
                                (car auto)
                                0))]
                      [vec (car k)]
                      [len (vector-length vec)])
                 (let loop ([i 0])
                   (or (= i len)
                       (let ([m (vector-ref vec i)])
                         (and (exact-nonnegative-integer? m) ; in case the vector is mutated
                              (< m n)
                              (loop (fx1+ i)))))))))))])))

;; Convert a prefab key to normalized, compact from
(define (normalize-prefab-key k keep-count?)
  (cond
   [(symbol? k) k]
   [else
    (let* ([name (car k)]
           [k (cdr k)]
           [count (if (and (pair? k)
                           (exact-nonnegative-integer? (car k)))
                      (car k)
                      #f)]
           [k (if count
                  (cdr k)
                  k)]
           [auto (if (and (pair? k)
                          (pair? (car k)))
                     (car k)
                     #f)]
           [k (if auto
                  (cdr k)
                  k)]
           [mutables (if (and (pair? k)
                              (vector? (car k)))
                         (car k)
                         #f)]
           [k (if mutables
                  (cdr k)
                  k)]
           [norm-auto (cond
                       [(not auto) #f]
                       [(eq? 0 (car auto)) #f]
                       [else auto])]
           [norm-mutables (cond
                           [(not mutables) #f]
                           [(zero? (vector-length mutables)) #f]
                           [else
                            (vector->immutable-vector
                             (chez:vector-sort (lambda (a b)
                                                 ;; Double-check exact integers, just in case
                                                 ;; a mutation happens; we'll have tou double-check
                                                 ;; that the result is still a prefab
                                                 (if (and (exact-nonnegative-integer? a)
                                                          (exact-nonnegative-integer? b))
                                                     (< a b)
                                                #f))
                                               mutables))])]
           [r (if (null? k)
                  '()
                  (normalize-prefab-key k #t))]
           [r (if norm-mutables
                  (cons norm-mutables
                        r)
                  r)]
           [r (if norm-auto
                  (cons norm-auto r)
                  r)])
      (if keep-count?
          (cons name (cons count r))
          (if (null? r)
              name
              (cons name r))))]))

(define (normalized-prefab-key/check who key field-count)
  (check who prefab-key? key)
  (unless (prefab-key-compatible-count? key field-count)
    (raise-arguments-error who
                           "mismatch between prefab key and field count"
                           "prefab key" key
                           "field count" field-count))
  (let ([norm-key (normalize-prefab-key key #f)])
    (unless (and (prefab-key? norm-key)
                 (prefab-key-compatible-count? norm-key field-count))
      (raise-arguments-error who
                             "prefab key mutated after initial check"
                             "prefab key" key))
    norm-key))

(define (prefab-key+size->prefab-key-tail key+size)
  (let ([key (car key+size)])
    (cond
     [(symbol? key)
      (list key (cdr key+size))]
     [else
      (cons* (car key)
             (- (cdr key+size)
                (prefab-key-count-explicit-fields key))
             (cdr key))])))

(define (prefab-key-count-explicit-fields key)
  ;; Count fields other than initial non-auto:
  (let loop ([k (cdr key)])
    (let* ([count (and (pair? k)
                       (exact-integer? (car k))
                       (car k))]
           [k (if count
                  (cdr k)
                  k)]
           [mutable (and (pair? k)
                         (pair? (car k))
                         (car k))]
           [k (if mutable
                  (cdr k)
                  k)]
           [k (if (and (pair? k)
                       (vector? (car k)))
                  (cdr k)
                  k)])
      (+ (or count 0)
         (if mutable (car mutable) 0)
         (cond
          [(null? k) 0]
          [else (loop (cdr k))])))))

(define (prefab-key->parent-prefab-key+count key)
  (cond
   [(symbol? key) #f]
   [else
    (let* ([k (cdr key)] ; skip name; non-auto count will no be present
           [k (if (and (pair? k)
                       (pair? (car k)))
                  (cdr k)
                  k)]
           [k (if (and (pair? k)
                       (vector? (car k)))
                  (cdr k)
                  k)])
      (if (null? k)
          #f
          ;; Normalize parent by dropping auto field count out:
          (let* ([name (car k)]
                 [count (cadr k)]
                 [rest-k (cddr k)]
                 [total-count (prefab-key-count-explicit-fields k)])
            (cond
             [(null? rest-k)
              (cons name total-count)]
             [else
              (cons (cons name rest-k) total-count)]))))]))

(define (derive-prefab-key name parent-key+size fields-count immutables auto-fields auto-val)
  (let* ([l (if parent-key+size
                (prefab-key+size->prefab-key-tail parent-key+size)
                '())]
         [l (let ([mutables (immutables->mutables immutables fields-count 0)])
              (if (fx= 0 (#%vector-length mutables))
                  l
                  (cons mutables l)))]
         [l (if (zero? auto-fields)
                l
                (cons (list auto-fields auto-val)
                      l))])
    (if (null? l)
        name
        (cons name l))))

(define (prefab-key-mutables prefab-key)
  (if (pair? prefab-key)
      (if (vector? (cadr prefab-key))
          (cadr prefab-key)
          (if (and (pair? (cddr prefab-key))
                   (vector? (caddr prefab-key)))
              (caddr prefab-key)
              '#()))
      '#()))

(define (encode-prefab-key+count-as-symbol prefab-key+count)
  (string->symbol (chez:format "~a" prefab-key+count)))

(define (immutables->mutables immutables init-count auto-count)
  (vector->immutable-vector
   (list->vector
    (let loop ([i 0])
      (cond
       [(fx= i init-count) (let loop ([i 0])
                             (cond
                              [(fx= i auto-count) null]
                              [else (cons (fx+ i init-count) (loop (fx+ i 1)))]))]
       [(#%memv i immutables) (loop (fx+ 1 i))]
       [else (cons i (loop (fx+ i 1)))])))))

(define (mutables->immutables mutables init-count)
  (let loop ([i 0])
    (cond
     [(fx= i init-count) '()]
     [else
      (let jloop ([j (vector-length mutables)])
        (cond
         [(fx= j 0) (cons i (loop (fx1+ i)))]
         [else
          (let ([j (fx1- j)])
            (if (eqv? i (vector-ref mutables j))
                (loop (fx1+ i))
                (jloop j)))]))])))
