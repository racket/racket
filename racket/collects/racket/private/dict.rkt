#lang racket/base

(require racket/private/generic ; to avoid circular dependencies
         racket/private/generic-methods
         racket/vector
         (only-in racket/private/hash paired-fold)
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
  (dict-implements? d 'dict-set!))

(define (dict-can-remove-keys? d)
  (unless (dict? d)
    (raise-argument-error 'dict-can-remove-keys? "dict?" d))
  (or (dict-implements? d 'dict-remove!)
      (dict-implements? d 'dict-remove)))

(define (dict-can-functional-set? d)
  (unless (dict? d)
    (raise-argument-error 'dict-can-functional-set? "dict?" d))
  (dict-implements? d 'dict-set))

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
  (unless (dict-implements? d 'dict-set!)
    (raise-support-error 'dict-ref! d))
  (define not-there (gensym))
  (define v (dict-ref d key not-there))
  (if (eq? not-there v)
      (let ([n (if (procedure? new) (new) new)])
        (dict-set! d key n)
        n)
      v))

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

(define (fallback-set*! d . pairs)
  (unless (dict-implements? d 'dict-set!)
    (raise-support-error 'dict-set*! d))
  (paired-fold 'dict-set*! pairs (void)
              (lambda (x k v)
                (dict-set! d k v))))

(define (fallback-set* d . pairs)
  (unless (dict-implements? d 'dict-set)
    (raise-support-error 'dict-set* d))
  (paired-fold 'dict-set* pairs d dict-set))

(define fallback-update!
  (case-lambda
   [(d key xform)
    (unless (dict-implements? d 'dict-set!)
      (raise-support-error 'dict-update! d))
    (dict-set! d key (xform (dict-ref d key)))]
   [(d key xform default)
    (unless (dict-implements? d 'dict-set!)
      (raise-support-error 'dict-update! d))
    (dict-set! d key (xform (dict-ref d key default)))]))

(define fallback-update
  (case-lambda
   [(d key xform)
    (unless (dict-implements? d 'dict-set)
      (raise-support-error 'dict-update d))
    (dict-set d key (xform (dict-ref d key)))]
   [(d key xform default)
    (unless (dict-implements? d 'dict-set)
      (raise-support-error 'dict-update d))
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

(define (fallback-copy d)
  (unless (dict-implements? d 'dict-clear dict-set!)
    (raise-support-error 'dict-copy d))
  (define d2 (dict-clear d))
  (for ([(k v) (in-dict d)])
    (dict-set! d2 k v))
  d2)

(define (assoc-clear d) '())

(define (fallback-clear d)
  (unless (dict-implements? d 'dict-remove)
    (raise-support-error 'dict-clear d))
  (for/fold ([d d]) ([k (in-dict-keys d)])
    (dict-remove d k)))

(define (fallback-clear! d)
  (unless (dict-implements? d 'dict-remove!)
    (raise-support-error 'dict-clear! d))
  (let loop ()
    (define i (dict-iterate-first d))
    (when i
      (dict-remove! d (dict-iterate-key d i))
      (loop))))

(define (fallback-empty? d)
  (not (dict-iterate-first d)))

(define (fallback-count d)
  (let loop ([n 0] [i (dict-iterate-first d)])
    (cond
      [(not i) n]
      [else (loop (add1 n) (dict-iterate-next d i))])))

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
  (dict gen:dict prop:gen:dict prop:gen:dict-methods dict? dict-implements?)
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
    (define dict-copy hash-copy)
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
    (define dict-copy hash-copy)
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
    (define dict-copy vector-copy)
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
    (define dict-copy vector-copy)
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
   (define dict-count fallback-count)
   (define dict-map fallback-map)
   (define dict-for-each fallback-for-each)
   (define dict-keys fallback-keys)
   (define dict-values fallback-values)
   (define dict->list fallback->list)
   (define dict-copy fallback-copy)
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
  (dict-copy dict)
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
         dict-copy
         dict-clear
         dict-clear!
         dict-empty?
         dict-implements?

         (rename-out [:in-dict in-dict]
                     [:in-dict-keys in-dict-keys]
                     [:in-dict-values in-dict-values])
         in-dict-pairs)
