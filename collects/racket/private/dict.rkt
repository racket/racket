#lang racket/base

(require racket/private/generic ; to avoid circular dependencies
         (for-syntax racket/base))

(define-generics (dict gen:dict prop:dict dict?
                       #:defined-table dict-def-table
                       #:defaults ()
                       ;; private version needs all kw args, in order
                       #:prop-defined-already? #f
                       #:define-contract #f)
  (dict-ref  dict key [default])
  (dict-set! dict key val)
  (dict-set  dict key val)
  (dict-remove! dict key)
  (dict-remove  dict key)
  (dict-count dict)
  (dict-iterate-first dict)
  (dict-iterate-next dict pos)
  (dict-iterate-key dict pos)
  (dict-iterate-value dict pos))

(define (assoc? v)
  (and (list? v) (andmap pair? v)))

(define (d:dict? v)
  (or (hash? v)
      (vector? v)
      (assoc? v)
      (dict? v)))

(define (dict-mutable? d)
  (if (d:dict? d)
      (or (and (or (hash? d)
                   (vector? d))
               (not (immutable? d)))
          (and (dict? d)
               (hash-ref (dict-def-table d) 'dict-set! #f)
               #t))
      (raise-argument-error 'dict-mutable? "dict?" d)))

(define (dict-can-remove-keys? d)
  (if (d:dict? d)
      (or (hash? d)
          (assoc? d)
          (and (dict? d)
               (or (hash-ref (dict-def-table d) 'dict-remove! #f)
                   (hash-ref (dict-def-table d) 'dict-remove #f))
               #t))
      (raise-argument-error 'dict-can-remove-keys? "dict?" d)))

(define (dict-can-functional-set? d)
  (if (d:dict? d)
      (or (and (hash? d) (immutable? d))
          (assoc? d)
          (and (dict? d)
               (hash-ref (dict-def-table d) 'dict-set #f)
               #t))
      (raise-argument-error 'dict-can-functional-set? "dict?" d)))

(define (dict-has-key? d k)
  (define not-there (gensym))
  (not (eq? not-there (d:dict-ref d k not-there))))

(define d:dict-ref
  (case-lambda
   [(d key)
    (cond
     [(hash? d) (hash-ref d key)]
     [(vector? d) (vector-ref d key)]
     [(assoc? d)
      (let ([a (assoc key d)])
        (if a
            (cdr a)
            (raise-mismatch-error 'dict-ref
                                  (format "no value for key: ~e in: "
                                          key)
                                  d)))]
     [(dict? d) (dict-ref d key)]
     [else
      (raise-argument-error 'dict-ref "dict?" 0 d key)])]
   [(d key default)
    (cond
     [(hash? d) (hash-ref d key default)]
     [(vector? d) (if (and (exact-nonnegative-integer? key)
                           (key . < . (vector-length d)))
                      (vector-ref d key)
                      (if (procedure? default)
                          (default)
                          default))]
     [(assoc? d)
      (let ([a (assoc key d)])
        (if a
            (cdr a)
            (if (procedure? default)
                (default)
                default)))]
     [(dict? d)
      (dict-ref d key default)]
     [else
      (raise-argument-error 'dict-ref "dict?" 0 d key default)])]))

(define (dict-ref! d key new)
  (define not-there (gensym))
  (define v (d:dict-ref d key not-there))
  (if (eq? not-there v)
      (let ([n (if (procedure? new) (new) new)])
        (d:dict-set! d key n)
        n)
      v))

(define (d:dict-set! d key val)
  (cond
   [(hash? d) (hash-set! d key val)]
   [(vector? d) (vector-set! d key val)]
   [(assoc? d)
    (raise-argument-error 'dict-set! "mutable-dict?" 0 d key val)]
   [(dict? d)
    (let ([s! (hash-ref (dict-def-table d) 'dict-set! #f)])
      (if s!
          (dict-set! d key val)
          (raise-argument-error 'dict-set! "mutable-dict?" 0 d key val)))]
   [else
    (raise-argument-error 'dict-set! "dict?" 0 d key val)]))

(define (dict-set*! d . pairs)
  (unless (even? (length pairs))
    (error 'dict-set*! "expected an even number of association elements, but received an odd number: ~e" pairs))
  (let loop ([pairs pairs])
    (unless (null? pairs)
      (d:dict-set! d (car pairs) (cadr pairs))
      (loop (cddr pairs)))))

(define (d:dict-set d key val)
  (cond
   [(hash? d) (hash-set d key val)]
   [(vector? d)
    (raise-argument-error 'dict-set "functional-update-dict?" 0  d key val)]
   [(assoc? d)
    (let loop ([xd d])
      (cond
       [(null? xd) (list (cons key val))]
       [else
        (let ([a (car xd)])
          (if (equal? (car a) key) 
              (cons (cons key val) (cdr xd))
              (cons a (loop (cdr xd)))))]))]
   [(dict? d)
    (let ([s (hash-ref (dict-def-table d) 'dict-set #f)])
      (if s
          (dict-set d key val)
          (raise-argument-error 'dict-set "functional-update-dict?" 0 d key val)))]
   [else
    (raise-argument-error 'dict-set "dict?" 0 d key val)]))

(define (dict-set* d . pairs)
    (unless (even? (length pairs))
      (error 'dict-set* "expected an even number of association elements, but received an odd number: ~e" pairs))
    (let loop ([d d]
               [pairs pairs])
      (if (null? pairs)
          d
          (loop (d:dict-set d (car pairs) (cadr pairs))
                (cddr pairs)))))

(define dict-update!
  (case-lambda
   [(d key xform)
    (d:dict-set! d key (xform (d:dict-ref d key)))]
   [(d key xform default)
    (d:dict-set! d key (xform (d:dict-ref d key default)))]))

(define dict-update
  (case-lambda
   [(d key xform)
    (d:dict-set d key (xform (d:dict-ref d key)))]
   [(d key xform default)
    (d:dict-set d key (xform (d:dict-ref d key default)))]))

(define (d:dict-remove! d key)
  (cond
   [(hash? d) (hash-remove! d key)]
   [(vector? d)
    (raise-argument-error 'dict-remove! "dict-with-removeable-keys?" 0 d key)]
   [(assoc? d)
    (raise-argument-error 'dict-remove! "mutable-dict?" 0 d key)]
   [(dict? d)
    (let ([r! (hash-ref (dict-def-table d) 'dict-remove! #f)])
      (if r!
          (dict-remove! d key)
          (raise-argument-error 'dict-remove! "mutable-dict-with-removable-keys?" 0 d key)))]
   [else
    (raise-argument-error 'dict-remove! "dict?" 0 d key)]))

(define (d:dict-remove d key)
  (cond
   [(hash? d) (hash-remove d key)]
   [(vector? d)
    (raise-argument-error 'dict-remove "dict-with-removeable-keys?" 0 d key)]
   [(assoc? d)
    (let loop ([xd d])
      (cond
       [(null? xd) null]
       [else
        (let ([a (car xd)])
          (if (equal? (car a) key) 
              (cdr xd)
              (cons a (loop (cdr xd)))))]))]
   [(dict? d)
    (let ([s (hash-ref (dict-def-table d) 'dict-remove #f)])
      (if s
          (dict-remove d key)
          (raise-argument-error 'dict-remove "dict-with-functionally-removeable-keys?" 0 d key)))]
   [else
    (raise-argument-error 'dict-remove "dict?" 0 d key)]))

(define (d:dict-count d)
  (cond
   [(hash? d) (hash-count d)]
   [(vector? d) (vector-length d)]
   [(assoc? d) (length d)]
   [(dict? d) (dict-count d)]
   [else
    (raise-argument-error 'dict-count "dict?" d)]))

(struct assoc-iter (head pos))

(define (d:dict-iterate-first d)
  (cond
   [(hash? d) (hash-iterate-first d)]
   [(vector? d) (if (zero? (vector-length d))
                    #f
                    0)]
   [(assoc? d) (if (null? d) #f (assoc-iter d d))]
   [(dict? d) (dict-iterate-first d)]
   [else
    (raise-argument-error 'dict-iterate-first "dict?" d)]))

(define (d:dict-iterate-next d i)
  (cond
   [(hash? d) (hash-iterate-next d i)]
   [(vector? d) (let ([len (vector-length d)])
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
                     i)]))]
   [(and (assoc-iter? i)
         (eq? d (assoc-iter-head i)))
    (let ([pos (cdr (assoc-iter-pos i))])
      (if (null? pos)
          #f
          (assoc-iter d pos)))]
   [(dict? d) (dict-iterate-next d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-next
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-argument-error 'dict-iterate-next "dict?" d)]))

(define (d:dict-iterate-key d i)
  (cond
   [(hash? d) (hash-iterate-key d i)]
   [(vector? d) i]
   [(and (assoc-iter? i) (eq? d (assoc-iter-head i))) (caar (assoc-iter-pos i))]
   [(dict? d) (dict-iterate-key d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-key
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-argument-error 'dict-iterate-key "dict?" d)]))

(define (d:dict-iterate-value d i)
  (cond
   [(hash? d) (hash-iterate-value d i)]
   [(vector? d) (vector-ref d i)]
   [(and (assoc-iter? i) (eq? d (assoc-iter-head i))) (cdar (assoc-iter-pos i))]
   [(dict? d) (dict-iterate-value d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-value
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-argument-error 'dict-iterate-value "dict?" d)]))

(define-sequence-syntax :in-dict
  (lambda () #'in-dict)
  (lambda (stx)
    (syntax-case stx ()
      [((key-id val-id) (_ dict-expr))
       #'[(key-id val-id)
          (:do-in ([(d) dict-expr])
                  (void)
                  ([i (d:dict-iterate-first d)])
                  i
                  ([key-id (d:dict-iterate-key d i)]
                   [val-id (d:dict-iterate-value d i)])
                  #t
                  #t
                  ((d:dict-iterate-next d i)))]]
      [_ #f])))

(define (in-dict d)
  (make-dict-sequence
   d
   (lambda (i)
     (values (d:dict-iterate-key d i)
             (d:dict-iterate-value d i)))
   (lambda (k v) #t)
   (lambda (i k v) #t)))

(define (in-dict-keys d)
  (make-dict-sequence
   d
   (lambda (i) (d:dict-iterate-key d i))
   (lambda (k) #t)
   (lambda (i k) #t)))

(define (in-dict-values d)
  (make-dict-sequence
   d
   (lambda (i) (d:dict-iterate-value d i))
   (lambda (v) #t)
   (lambda (i v) #t)))

(define (in-dict-pairs d)
  (make-dict-sequence
   d
   (lambda (i)
     (cons (d:dict-iterate-key d i)
           (d:dict-iterate-value d i)))
   (lambda (p) #t)
   (lambda (i p) #t)))

(define (make-dict-sequence d get val-true val+pos-true)
  (make-do-sequence
   (lambda ()
     (values get
             (lambda (i) (d:dict-iterate-next d i))
             (d:dict-iterate-first d)
             (lambda (i) i)
             val-true
             val+pos-true))))

(define (dict-map d f)
  (for/list ([(k v) (in-dict d)])
    (f k v)))

(define (dict-for-each d f)
  (for ([(k v) (in-dict d)])
    (f k v)))

(define (dict-keys d)
  (for/list ([k (in-dict-keys d)])
    k))

(define (dict-values d)
  (for/list ([v (in-dict-values d)])
    v))

(define (dict->list d)
  (for/list ([k*v (in-dict-pairs d)])
    k*v))

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

(struct custom-hash (table make-box)
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set! custom-hash-set!)
   (define dict-remove! custom-hash-remove!)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)]
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
   (define dict-iterate-value custom-hash-iterate-value)])
          
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
         (rename-out
           [d:dict?              dict?]
           [d:dict-ref           dict-ref]
           [d:dict-set!          dict-set!]
           [d:dict-set           dict-set]
           [d:dict-remove!       dict-remove!]
           [d:dict-remove        dict-remove]
           [d:dict-count         dict-count]
           [d:dict-iterate-first dict-iterate-first]
           [d:dict-iterate-next  dict-iterate-next]
           [d:dict-iterate-key   dict-iterate-key]
           [d:dict-iterate-value dict-iterate-value])
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
         (rename-out [create-custom-hash make-custom-hash]
                     [create-immutable-custom-hash make-immutable-custom-hash])
         make-weak-custom-hash

         in-dict
         in-dict-keys
         in-dict-values
         in-dict-pairs)
  
