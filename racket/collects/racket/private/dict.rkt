#lang racket/base

(require racket/private/generic ; to avoid circular dependencies
         racket/private/generic-methods
         (for-syntax racket/base))

(define (assoc? v)
  (and (list? v) (andmap pair? v)))

(define (immutable-hash? v)
  (and (hash? v) (immutable? v)))

(define (mutable-hash? v)
  (and (hash? v) (not (immutable? v))))

(define (immutable-vector? v)
  (and (vector? v) (immutable? v)))

(define (mutable-vector? v)
  (and (vector? v) (not (immutable? v))))

(define (dict-mutable? d)
  (unless (dict? d)
    (raise-argument-error 'dict-mutable? "dict?" d))
  (dict-supports? d 'dict-set!))

(define (dict-can-remove-keys? d)
  (unless (dict? d)
    (raise-argument-error 'dict-can-remove-keys? "dict?" d))
  (or (dict-supports? d 'dict-remove!)
      (dict-supports? d 'dict-remove)))

(define (dict-can-functional-set? d)
  (unless (dict? d)
    (raise-argument-error 'dict-can-functional-set? "dict?" d))
  (dict-supports? d 'dict-set))

(define (fallback-has-key? d k)
  (define not-there (gensym))
  (not (eq? not-there (dict-ref d k not-there))))

(define vector-ref-as-dict
  (case-lambda
    [(d key) (vector-ref d key)]
    [(d key default)
     (if (and (exact-nonnegative-integer? key)
              (key . < . (vector-length d)))
         (vector-ref d key)
         (if (procedure? default)
             (default)
             default))]))

(define no-arg (gensym))
(define (assoc-ref d key [default no-arg])
  (unless (assoc? d)
    (raise-argument-error 'dict-ref "dict?" d))
  (cond
    [(assoc key d) => cdr]
    [(eq? default no-arg)
     (raise-mismatch-error 'dict-ref
                           (format "no value for key: ~e in: " key)
                           d)]
    [(procedure? default) (default)]
    [else default]))

(define (fallback-ref! d key new)
  (unless (dict-supports? d 'dict-set!)
    (raise-argument-error 'dict-ref! "(dict-supports/c 'dict-set!)" d))
  (define not-there (gensym))
  (define v (dict-ref d key not-there))
  (if (eq? not-there v)
      (let ([n (if (procedure? new) (new) new)])
        (dict-set! d key n)
        n)
      v))

(define (fallback-set*! d . pairs0)
  (unless (dict-supports? d 'dict-set!)
    (raise-argument-error 'dict-set*! "(dict-supports/c 'dict-set!)" d))
  (let loop ([pairs pairs0])
    (cond
      [(null? pairs) (void)]
      [(null? (cdr pairs))
       (raise-arguments-error
        'dict-set*!
        "expected an even number of association elements, but received an odd number"
        "association elements"
        pairs0)]
      [else
       (dict-set! d (car pairs) (cadr pairs))
       (loop (cddr pairs))])))

(define (assoc-set d key val)
  (unless (assoc? d)
    (raise-argument-error 'dict-set "dict?" d))
  (let loop ([xd d])
    (cond
     [(null? xd) (list (cons key val))]
     [else
      (let ([a (car xd)])
        (if (equal? (car a) key) 
            (cons (cons key val) (cdr xd))
            (cons a (loop (cdr xd)))))])))

(define (fallback-set* d . pairs0)
  (unless (dict-supports? d 'dict-set)
    (raise-argument-error 'dict-set* "(dict-supports/c 'dict-set)" d))
  (let loop ([d d]
             [pairs pairs0])
    (cond
      [(null? pairs) d]
      [(null? (cdr pairs))
       (raise-arguments-error
        'dict-set*
        "expected an even number of association elements, but received an odd number"
        "association elements"
        pairs0)]
      [else
       (loop (dict-set d (car pairs) (cadr pairs))
             (cddr pairs))])))

(define fallback-update!
  (case-lambda
   [(d key xform)
    (unless (dict-supports? d 'dict-set!)
      (raise-argument-error 'dict-update! "(dict-supports/c 'dict-set!)" d))
    (dict-set! d key (xform (dict-ref d key)))]
   [(d key xform default)
    (unless (dict-supports? d 'dict-set!)
      (raise-argument-error 'dict-update! "(dict-supports/c 'dict-set!)" d))
    (dict-set! d key (xform (dict-ref d key default)))]))

(define fallback-update
  (case-lambda
   [(d key xform)
    (unless (dict-supports? d 'dict-set)
      (raise-argument-error 'dict-update "(dict-supports/c 'dict-set)" d))
    (dict-set d key (xform (dict-ref d key)))]
   [(d key xform default)
    (unless (dict-supports? d 'dict-set)
      (raise-argument-error 'dict-update "(dict-supports/c 'dict-set)" d))
    (dict-set d key (xform (dict-ref d key default)))]))

(define (assoc-remove d key)
  (unless (assoc? d)
    (raise-argument-error 'dict-remove "dict?" d))
  (let loop ([xd d])
    (cond
     [(null? xd) null]
     [else
      (let ([a (car xd)])
        (if (equal? (car a) key) 
            (cdr xd)
            (cons a (loop (cdr xd)))))])))

(define (vector-iterate-first d)
  (if (zero? (vector-length d)) #f 0))

(define (vector-iterate-next d i)
  (let ([len (vector-length d)])
    (cond
     [(and (exact-nonnegative-integer? i)
           (i . < . len))
      (let ([i (add1 i)])
        (if (= i len)
            #f
            i))]
     [else
      (raise-mismatch-error 
       'dict-iterate-next
       "invalid iteration position for vector: " 
       i)])))

(define (vector-iterate-key d i) i)

(define vector-iterate-value vector-ref)

(define (assoc-count d)
  (unless (assoc? d)
    (raise-argument-error 'dict-count "dict?" d))
  (length d))

(struct assoc-iter (head pos))

(define (assoc-iterate-first d)
  (unless (assoc? d)
    (raise-argument-error 'dict-iterate-first "dict?" d))
  (if (null? d) #f (assoc-iter d d)))

(define (assoc-iterate-next d i)
  (cond
    [(and (assoc-iter? i)
          (eq? d (assoc-iter-head i)))
     (let ([pos (cdr (assoc-iter-pos i))])
       (if (null? pos)
           #f
           (assoc-iter d pos)))]
    [(assoc? d)
     (raise-mismatch-error 
      'dict-iterate-next
      "invalid iteration position for association list: " 
      i)]
    [else (raise-argument-error 'dict-iterate-next "dict?" d)]))

(define (assoc-iterate-key d i)
  (cond
    [(and (assoc-iter? i) (eq? d (assoc-iter-head i)))
     (caar (assoc-iter-pos i))]
    [(assoc? d)
     (raise-mismatch-error 
      'dict-iterate-key
      "invalid iteration position for association list: " 
      i)]
    [else (raise-argument-error 'dict-iterate-key "dict?" d)]))

(define (assoc-iterate-value d i)
  (cond
    [(and (assoc-iter? i) (eq? d (assoc-iter-head i)))
     (cdar (assoc-iter-pos i))]
    [(assoc? d)
     (raise-mismatch-error 
      'dict-iterate-value
      "invalid iteration position for association list: " 
      i)]
    [else (raise-argument-error 'dict-iterate-value "dict?" d)]))

(define (vector-has-key? vec key)
  (and (exact-nonnegative-integer? key)
       (< key (vector-length vec))))

(define (vector-map-as-dict vec proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-argument-error 'dict-map "(procedure-arity-includes/c 2)" proc))
  (for/list ([k (in-naturals)] [v (in-vector vec)])
    (proc k v)))

(define (vector-for-each vec proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-argument-error 'dict-for-each "(procedure-arity-includes/c 2)" proc))
  (for ([k (in-naturals)] [v (in-vector vec)])
    (proc k v)))

(define (vector-keys vec)
  (build-list (vector-length vec) values))

(define (vector->assoc vec)
  (for/list ([k (in-naturals)] [v (in-vector vec)])
    (cons k v)))

(define (vector-empty? vec)
  (zero? (vector-length vec)))

(define (assoc-has-key? d key)
  (unless (assoc? d)
    (raise-argument-error 'dict-has-key? "dict?" d))
  (pair? (assoc key d)))

(define (assoc-map d proc)
  (for/list ([x (in-list d)])
    (unless (pair? x)
      (raise-argument-error 'dict-map "dict?" d))
    (proc (car x) (cdr x))))

(define (assoc-for-each d proc)
  (for ([x (in-list d)])
    (unless (pair? x)
      (raise-argument-error 'dict-for-each "dict?" d))
    (proc (car x) (cdr x))))

(define (assoc-keys d)
  (for/list ([x (in-list d)])
    (unless (pair? x)
      (raise-argument-error 'dict-keys "dict?" d))
    (car x)))

(define (assoc-values d)
  (for/list ([x (in-list d)])
    (unless (pair? x)
      (raise-argument-error 'dict-values "dict?" d))
    (cdr x)))

(define (assoc-clear d) '())

(define (fallback-clear d)
  (unless (dict-supports? d 'dict-remove)
    (raise-argument-error 'dict-clear "(dict-supports/c 'dict-remove)" d))
  (for/fold ([d d]) ([k (in-dict-keys d)])
    (dict-remove d k)))

(define (fallback-clear! d)
  (unless (dict-supports? d 'dict-remove!)
    (raise-argument-error 'dict-clear! "(dict-supports/c 'dict-remove!)" d))
  (let loop ()
    (define i (dict-iterate-first d))
    (when i
      (dict-remove! d (dict-iterate-key d i))
      (loop))))

(define (fallback-empty? d)
  (if (dict-iterate-first d) #t #f))

(define (fallback-map d f)
  (for/list ([(k v) (:in-dict d)])
    (f k v)))

(define (fallback-for-each d f)
  (for ([(k v) (:in-dict d)])
    (f k v)))

(define (fallback-keys d)
  (for/list ([k (:in-dict-keys d)])
    k))

(define (fallback-values d)
  (for/list ([v (:in-dict-values d)])
    v))

(define (fallback->list d)
  (for/list ([k*v (in-dict-pairs d)])
    k*v))

(define-primitive-generics
  (dict gen:dict prop:gen:dict prop:gen:dict-methods dict? dict-supports?)
  #:fast-defaults
  ([mutable-hash? mutable-hash?
    (define dict-ref hash-ref)
    (define dict-set! hash-set!)
    (define dict-remove! hash-remove!)
    (define dict-count hash-count)
    (define dict-iterate-first hash-iterate-first)
    (define dict-iterate-next hash-iterate-next)
    (define dict-iterate-key hash-iterate-key)
    (define dict-iterate-value hash-iterate-value)
    (define dict-has-key? hash-has-key?)
    (define dict-ref! hash-ref!)
    (define dict-set*! hash-set*!)
    (define dict-update! hash-update!)
    (define dict-map hash-map)
    (define dict-for-each hash-for-each)
    (define dict-keys hash-keys)
    (define dict-values hash-values)
    (define dict->list hash->list)
    (define dict-empty? hash-empty?)
    (define dict-clear hash-clear)
    (define dict-clear! hash-clear!)]
   [immutable-hash? immutable-hash?
    (define dict-ref hash-ref)
    (define dict-set hash-set)
    (define dict-remove hash-remove)
    (define dict-count hash-count)
    (define dict-iterate-first hash-iterate-first)
    (define dict-iterate-next hash-iterate-next)
    (define dict-iterate-key hash-iterate-key)
    (define dict-iterate-value hash-iterate-value)
    (define dict-has-key? hash-has-key?)
    (define dict-set* hash-set*)
    (define dict-update hash-update)
    (define dict-map hash-map)
    (define dict-for-each hash-for-each)
    (define dict-keys hash-keys)
    (define dict-values hash-values)
    (define dict->list hash->list)
    (define dict-empty? hash-empty?)
    (define dict-clear hash-clear)]
   [mutable-vector? mutable-vector?
    (define dict-ref vector-ref-as-dict)
    (define dict-set! vector-set!)
    (define dict-count vector-length)
    (define dict-iterate-first vector-iterate-first)
    (define dict-iterate-next vector-iterate-next)
    (define dict-iterate-key vector-iterate-key)
    (define dict-iterate-value vector-iterate-value)
    (define dict-has-key? vector-has-key?)
    (define dict-map vector-map-as-dict)
    (define dict-for-each vector-for-each)
    (define dict-keys vector-keys)
    (define dict-values vector->list)
    (define dict->list vector->assoc)
    (define dict-empty? vector-empty?)]
   [immutable-vector? immutable-vector?
    (define dict-ref vector-ref-as-dict)
    (define dict-count vector-length)
    (define dict-iterate-first vector-iterate-first)
    (define dict-iterate-next vector-iterate-next)
    (define dict-iterate-key vector-iterate-key)
    (define dict-iterate-value vector-iterate-value)
    (define dict-has-key? vector-has-key?)
    (define dict-map vector-map-as-dict)
    (define dict-for-each vector-for-each)
    (define dict-keys vector-keys)
    (define dict-values vector->list)
    (define dict->list vector->assoc)
    (define dict-empty? vector-empty?)]
   [assoc? list?
    (define dict-ref assoc-ref)
    (define dict-set assoc-set)
    (define dict-remove assoc-remove)
    (define dict-count assoc-count)
    (define dict-iterate-first assoc-iterate-first)
    (define dict-iterate-next assoc-iterate-next)
    (define dict-iterate-key assoc-iterate-key)
    (define dict-iterate-value assoc-iterate-value)
    (define dict-has-key? assoc-has-key?)
    (define dict-map assoc-map)
    (define dict-for-each assoc-for-each)
    (define dict-keys assoc-keys)
    (define dict-values assoc-values)
    (define dict->list values)
    (define dict-empty? null?)
    (define dict-clear assoc-clear)])
  #:defaults ()
  #:fallbacks
  [(define dict-has-key? fallback-has-key?)
   (define dict-ref! fallback-ref!)
   (define dict-set*! fallback-set*!)
   (define dict-set* fallback-set*)
   (define dict-update! fallback-update!)
   (define dict-update fallback-update)
   (define dict-map fallback-map)
   (define dict-for-each fallback-for-each)
   (define dict-keys fallback-keys)
   (define dict-values fallback-values)
   (define dict->list fallback->list)
   (define dict-empty? fallback-empty?)
   (define dict-clear fallback-clear)
   (define dict-clear! fallback-clear!)]
  #:derive-properties ()
  (dict-ref  dict key [default])
  (dict-set! dict key val)
  (dict-set  dict key val)
  (dict-remove! dict key)
  (dict-remove  dict key)
  (dict-count dict)
  (dict-iterate-first dict)
  (dict-iterate-next dict pos)
  (dict-iterate-key dict pos)
  (dict-iterate-value dict pos)
  (dict-has-key? dict key)
  (dict-ref! dict key default)
  (dict-set*! dict . pairs)
  (dict-set* dict . pairs)
  (dict-update! dict key proc [default])
  (dict-update dict key proc [default])
  (dict-map dict proc)
  (dict-for-each dict proc)
  (dict-keys dict)
  (dict-values dict)
  (dict->list dict)
  (dict-empty? dict)
  (dict-clear dict)
  (dict-clear! dict))

(define (check-method who v i req? name arity [alt #f])
  (define m (vector-ref v i))
  (unless (or (and (not req?) (not m))
              (and (procedure? m)
                   (procedure-arity-includes? m arity)
                   (or (not alt)
                       (procedure-arity-includes? m alt))))
    (raise-arguments-error
     who
     (format
      "method at index ~a (~a) must be~a a procedure that accepts ~a ~a~a"
      i
      name
      (if req? "" " #f or")
      arity
      (if (= 1 arity) "argument" "arguments")
      (if alt
          (format " and ~a ~a"
                  alt
                  (if (= 1 alt) "argument" "arguments"))
          ""))
     name
     m)))

(define (guard-for-prop:dict v info)
  (unless (and (vector? v) (= (vector-length v) 10))
    (raise-argument-error 'guard-for-prop:dict "a vector of length 10" v))
  (check-method 'guard-for-prop:dict v 0 #t "dict-ref" 2 3)
  (check-method 'guard-for-prop:dict v 1 #f "dict-set!" 3)
  (check-method 'guard-for-prop:dict v 2 #f "dict-set" 3)
  (check-method 'guard-for-prop:dict v 3 #f "dict-remove!" 2)
  (check-method 'guard-for-prop:dict v 4 #f "dict-remove" 2)
  (check-method 'guard-for-prop:dict v 5 #t "dict-count" 1)
  (check-method 'guard-for-prop:dict v 6 #t "dict-iterate-first" 1)
  (check-method 'guard-for-prop:dict v 7 #t "dict-iterate-next" 2)
  (check-method 'guard-for-prop:dict v 8 #t "dict-iterate-key" 2)
  (check-method 'guard-for-prop:dict v 9 #t "dict-iterate-value" 2)
  v)

(define (prop:dict->gen:dict v)
  (generic-method-table gen:dict
    (define dict-ref (vector-ref v 0))
    (define dict-set! (vector-ref v 1))
    (define dict-set (vector-ref v 2))
    (define dict-remove! (vector-ref v 3))
    (define dict-remove (vector-ref v 4))
    (define dict-count (vector-ref v 5))
    (define dict-iterate-first (vector-ref v 6))
    (define dict-iterate-next (vector-ref v 7))
    (define dict-iterate-key (vector-ref v 8))
    (define dict-iterate-value (vector-ref v 9))))

(define-values (prop:dict dict-via-prop? prop:dict-methods)
  (make-struct-type-property
   'dict
   guard-for-prop:dict
   (list (cons prop:gen:dict prop:dict->gen:dict))
   #t))

(define-sequence-syntax :in-dict
  (lambda () #'in-dict)
  (lambda (stx)
    (syntax-case stx ()
      [((key-id val-id) (_ dict-expr))
       #'[(key-id val-id)
          (:do-in ([(d) dict-expr])
                  (unless (dict? d)
                    (raise-argument-error 'in-dict "dict?" d))
                  ([i (dict-iterate-first d)])
                  i
                  ([(key-id) (dict-iterate-key d i)]
                   [(val-id) (dict-iterate-value d i)])
                  #t
                  #t
                  ((dict-iterate-next d i)))]]
      [_ #f])))

(define-sequence-syntax :in-dict-keys
  (lambda () #'in-dict-keys)
  (lambda (stx)
    (syntax-case stx ()
      [((key-id) (_ dict-expr))
       #'[(key-id)
          (:do-in ([(d) dict-expr])
                  (unless (dict? d)
                    (raise-argument-error 'in-dict-keys "dict?" d))
                  ([i (dict-iterate-first d)])
                  i
                  ([(key-id) (dict-iterate-key d i)])
                  #t
                  #t
                  ((dict-iterate-next d i)))]]
      [_ #f])))

(define-sequence-syntax :in-dict-values
  (lambda () #'in-dict-values)
  (lambda (stx)
    (syntax-case stx ()
      [((val-id) (_ dict-expr))
       #'[(key-id val-id)
          (:do-in ([(d) dict-expr])
                  (unless (dict? d)
                    (raise-argument-error 'in-dict-values "dict?" d))
                  ([i (dict-iterate-first d)])
                  i
                  ([(val-id) (dict-iterate-value d i)])
                  #t
                  #t
                  ((dict-iterate-next d i)))]]
      [_ #f])))

(define (in-dict d)
  (make-dict-sequence
   d
   (lambda (i)
     (values (dict-iterate-key d i)
             (dict-iterate-value d i)))
   (lambda (k v) #t)
   (lambda (i k v) #t)))

(define (in-dict-keys d)
  (make-dict-sequence
   d
   (lambda (i) (dict-iterate-key d i))
   (lambda (k) #t)
   (lambda (i k) #t)))

(define (in-dict-values d)
  (make-dict-sequence
   d
   (lambda (i) (dict-iterate-value d i))
   (lambda (v) #t)
   (lambda (i v) #t)))

(define (in-dict-pairs d)
  (make-dict-sequence
   d
   (lambda (i)
     (cons (dict-iterate-key d i)
           (dict-iterate-value d i)))
   (lambda (p) #t)
   (lambda (i p) #t)))

(define (make-dict-sequence d get val-true val+pos-true)
  (make-do-sequence
   (lambda ()
     (values get
             (lambda (i) (dict-iterate-next d i))
             (dict-iterate-first d)
             (lambda (i) i)
             val-true
             val+pos-true))))

;; ----------------------------------------

(struct hash-box (key))

(define custom-hash-ref
  (case-lambda
   [(d k) (hash-ref (custom-hash-table d)
                    ((custom-hash-make-box d) k)
                    (lambda ()
                      (raise-mismatch-error
                       'dict-ref
                       "no value found for key: "
                       k)))]
   [(d k fail) (hash-ref (custom-hash-table d)
                         ((custom-hash-make-box d) k)
                         fail)]))

(define (custom-hash-set! d k v)
  (hash-set! (custom-hash-table d) 
             ((custom-hash-make-box d) k)
             v))

(define (custom-hash-set d k v)
  (let ([table (hash-set (custom-hash-table d) 
                         ((custom-hash-make-box d) k)
                         v)])
    (immutable-custom-hash table 
                           (custom-hash-make-box d))))

(define (custom-hash-remove! d k)
  (hash-remove! (custom-hash-table d)
                ((custom-hash-make-box d) k)))

(define (custom-hash-remove d k)
  (let ([table (hash-remove (custom-hash-table d)
                            ((custom-hash-make-box d) k))])
    (immutable-custom-hash table 
                           (custom-hash-make-box d))))

(define (custom-hash-count d)
  (hash-count (custom-hash-table d)))

(define (custom-hash-iterate-first d)
  (hash-iterate-first (custom-hash-table d)))

(define (custom-hash-iterate-next d i)
  (hash-iterate-next (custom-hash-table d) i))

(define (custom-hash-iterate-key d i)
  (hash-box-key (hash-iterate-key (custom-hash-table d) i)))

(define (custom-hash-iterate-value d i)
  (hash-iterate-value (custom-hash-table d) i))

(define (custom-hash-has-key? d key)
  (hash-has-key? (custom-hash-table d) ((custom-hash-make-box d) key)))

(define (custom-hash-ref! d key new)
  (hash-ref! (custom-hash-table d) ((custom-hash-make-box d) key) new))

(define (custom-hash-set*! d . pairs0)
  (define table (custom-hash-table d))
  (define make-box (custom-hash-make-box d))
  (let loop ([pairs pairs0])
    (cond
      [(null? pairs) (void)]
      [(null? (cdr pairs))
       (raise-arguments-error
        'dict-set*!
        "expected an even number of association elements, but received an odd number"
        "association elements"
        pairs0)]
      [else
       (hash-set! table (make-box (car pairs)) (cadr pairs))
       (loop (cddr pairs))])))

(define (custom-hash-set* d . pairs0)
  (define make-box (custom-hash-make-box d))
  (let loop ([table (custom-hash-table d)]
             [pairs pairs0])
    (cond
      [(null? pairs) (immutable-custom-hash table make-box)]
      [(null? (cdr pairs))
       (raise-arguments-error
        'dict-set*
        "expected an even number of association elements, but received an odd number"
        "association elements"
        pairs0)]
      [else
       (loop (hash-set table (make-box (car pairs)) (cadr pairs))
             (cddr pairs))])))

(define custom-hash-update!
  (case-lambda
    [(d key proc)
     (define make-box (custom-hash-make-box d))
     (hash-update! (custom-hash-table d) (make-box key) proc)]
    [(d key proc new)
     (define make-box (custom-hash-make-box d))
     (hash-update! (custom-hash-table d) (make-box key) proc new)]))

(define custom-hash-update
  (case-lambda
    [(d key proc)
     (define make-box (custom-hash-make-box d))
     (define table (hash-update (custom-hash-table d) (make-box key) proc))
     (immutable-custom-hash table make-box)]
    [(d key proc new)
     (define make-box (custom-hash-make-box d))
     (define table (hash-update (custom-hash-table d) (make-box key) proc new))
     (immutable-custom-hash table make-box)]))

(define (custom-hash-map d proc)
  (hash-map (custom-hash-table d)
            (lambda (boxed val)
              (proc (hash-box-key boxed) val))))

(define (custom-hash-for-each d proc)
  (hash-for-each (custom-hash-table d)
                 (lambda (boxed val)
                   (proc (hash-box-key boxed) val))))

;; custom-hash-keys, -values, and ->list:
;; We use for/fold rather than for/list to save on the final reverse
;; because the order is nondeterministic anyway.

(define (custom-hash-keys d)
  (for/fold ([keys '()]) ([boxed (in-hash-keys (custom-hash-table d))])
    (cons (hash-box-key boxed) keys)))

(define (custom-hash-values d)
  (for/fold ([vals '()]) ([val (in-hash-values (custom-hash-table d))])
    (cons val vals)))

(define (custom-hash->list d)
  (for/fold ([pairs '()]) ([(boxed val) (in-hash (custom-hash-table d))])
    (cons (cons (hash-box-key boxed) val) pairs)))

(define (custom-hash-empty? d)
  (hash-empty? (custom-hash-table d)))

(define (custom-hash-clear d)
  (if (immutable-custom-hash? d)
      (immutable-custom-hash (hash-clear (custom-hash-table d))
                             (custom-hash-make-box d))
      (custom-hash (hash-clear (custom-hash-table d))
                   (custom-hash-make-box d))))

(define (custom-hash-clear! d)
  (set-custom-hash-table! d (hash-clear (custom-hash-table d))))

(struct custom-hash ([table #:mutable] make-box)
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set! custom-hash-set!)
   (define dict-remove! custom-hash-remove!)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)
   (define dict-has-key? custom-hash-has-key?)
   (define dict-ref! custom-hash-ref!)
   (define dict-set*! custom-hash-set*!)
   (define dict-update! custom-hash-update!)
   (define dict-map custom-hash-map)
   (define dict-for-each custom-hash-for-each)
   (define dict-keys custom-hash-keys)
   (define dict-values custom-hash-values)
   (define dict->list custom-hash->list)
   (define dict-empty? custom-hash-empty?)
   (define dict-clear custom-hash-clear)
   (define dict-clear! custom-hash-clear!)]
  #:methods gen:equal+hash
  [(define (equal-proc a b recur)
     (and (recur (custom-hash-make-box a)
                 (custom-hash-make-box b))
          (recur (custom-hash-table a)
                 (custom-hash-table b))))
   (define (hash-proc a recur)
     (recur (custom-hash-table a)))
   (define (hash2-proc a recur)
     (recur (custom-hash-table a)))])

(struct immutable-custom-hash custom-hash ()
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set custom-hash-set)
   (define dict-remove custom-hash-remove)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)
   (define dict-has-key? custom-hash-has-key?)
   (define dict-set* custom-hash-set*)
   (define dict-update custom-hash-update)
   (define dict-map custom-hash-map)
   (define dict-for-each custom-hash-for-each)
   (define dict-keys custom-hash-keys)
   (define dict-values custom-hash-values)
   (define dict->list custom-hash->list)
   (define dict-empty? custom-hash-empty?)
   (define dict-clear custom-hash-clear)])
          
(define-values (create-custom-hash 
                create-immutable-custom-hash
                make-weak-custom-hash)
  (let ([mk
         (lambda (hash hash2 =? who make-custom-hash table wrap-make-box)
           (unless (and (procedure? =?)
                        (procedure-arity-includes? =? 2))
             (raise-argument-error who "(any/c any/c . -> . any/c)" =?))
           (unless (and (procedure? hash)
                        (procedure-arity-includes? hash 1))
             (raise-argument-error who "(any/c . -> . exact-integer?)" hash))
           (unless (and (procedure? hash2)
                        (procedure-arity-includes? hash2 1))
             (raise-argument-error who "(any/c . -> . exact-integer?)" hash2))
           (let ()
             (struct box hash-box ()
               #:methods gen:equal+hash
               [(define (equal-proc a b recur)
                  (=? (hash-box-key a) (hash-box-key b)))
                (define (hash-proc v recur)
                  (hash (hash-box-key v)))
                (define (hash2-proc v recur)
                  (hash2 (hash-box-key v)))])
             (make-custom-hash table (wrap-make-box box))))])
    (let ([make-custom-hash 
           (lambda (=? hash [hash2 (lambda (v) 10001)])
             (mk hash hash2 =? 'make-custom-hash custom-hash (make-hash) values))]
          [make-immutable-custom-hash 
           (lambda (=? hash [hash2 (lambda (v) 10001)])
             (mk hash hash2 =? 'make-immutable-custom-hash immutable-custom-hash #hash() values))]
          [make-weak-custom-hash 
           (lambda (=? hash [hash2 (lambda (v) 10001)])
             (mk hash hash2 =? 'make-weak-custom-hash custom-hash (make-weak-hash)
                 (lambda (make-box)
                   (let ([ht (make-weak-hasheq)])
                     (lambda (v)
                       (let ([e (hash-ref ht v #f)])
                         (if e
                             (ephemeron-value e)
                             (let ([b (make-box v)])
                               (hash-set! ht v (make-ephemeron v b))
                               b))))))))])
      (values make-custom-hash 
              make-immutable-custom-hash
              make-weak-custom-hash))))

;; --------------------

(provide gen:dict
         prop:dict
         dict?
         dict-ref
         dict-set!
         dict-set
         dict-remove!
         dict-remove
         dict-count
         dict-iterate-first
         dict-iterate-next
         dict-iterate-key
         dict-iterate-value
         dict-mutable?
         dict-can-remove-keys?
         dict-can-functional-set?
         dict-has-key?
         dict-ref!
         dict-set*!
         dict-set*
         dict-update!
         dict-update
         dict-map
         dict-for-each
         dict-keys
         dict-values
         dict->list
         dict-clear
         dict-clear!
         dict-empty?
         dict-supports?
         (rename-out [create-custom-hash make-custom-hash]
                     [create-immutable-custom-hash make-immutable-custom-hash])
         make-weak-custom-hash

         (rename-out [:in-dict in-dict]
                     [:in-dict-keys in-dict-keys]
                     [:in-dict-values in-dict-values])
         in-dict-pairs)
  
