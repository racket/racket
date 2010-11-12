#lang racket/base
(require (for-syntax racket/base))

(define (dict-property-guard v info)
  (check-dict-vector 'prop:dict "dictionary property" v)
  v)

(define (check-dict-vector caller desc v)
  (check-vector*
   caller desc v
   (list check-dict-ref
         check-dict-set!
         check-dict-set
         check-dict-remove
         check-dict-remove!
         check-dict-count
         check-dict-iterate-first
         check-dict-iterate-next
         check-dict-iterate-key
         check-dict-iterate-value)))

(define (check-vector* caller desc v checkers)
  (unless (vector? v)
    (contract-error
     "~a: expected ~a to be a vector, but got: ~e"
     caller desc v))
  (let* ([expected (length checkers)]
         [actual (vector-length v)])
    (unless (= expected actual)
      (contract-error
       (string-append
        "~a: expected ~a to be a vector of ~a elements, "
        "but got ~a elements in: ~e")
       caller desc expected actual v)))
  (for ([elem (in-vector v)] [checker (in-list checkers)] [index (in-naturals)])
    (checker caller (format "element ~a of ~a" index desc) elem)))

(define (check-dict-ref caller desc v)
  (check-function/arity caller (describe "ref" desc) v 2 3))
(define (check-dict-set! caller desc v)
  (check-optional-function/arity caller (describe "set!" desc) v 3))
(define (check-dict-set caller desc v)
  (check-optional-function/arity caller (describe "set" desc) v 3))
(define (check-dict-remove! caller desc v)
  (check-optional-function/arity caller (describe "remove!" desc) v 2))
(define (check-dict-remove caller desc v)
  (check-optional-function/arity caller (describe "remove" desc) v 2))
(define (check-dict-count caller desc v)
  (check-function/arity caller (describe "count" desc) v 1))
(define (check-dict-iterate-first caller desc v)
  (check-function/arity caller (describe "iterate-first" desc) v 1))
(define (check-dict-iterate-next caller desc v)
  (check-function/arity caller (describe "iterate-next" desc) v 2))
(define (check-dict-iterate-key caller desc v)
  (check-function/arity caller (describe "iterate-key" desc) v 2))
(define (check-dict-iterate-value caller desc v)
  (check-function/arity caller (describe "iterate-value" desc) v 2))

(define (describe name desc)
  (format "~a (~a)" name desc))

(define (check-function/arity caller desc v . arities)
  (unless (procedure? v)
    (contract-error
     "~a: expected ~a to be a function, but got: ~e"
     caller desc v))
  (for ([arity (in-list arities)])
    (unless (procedure-arity-includes? v arity)
      (contract-error
       "~a: expected ~a to be a function that accepts ~a arguments, but got: ~e"
       caller desc arity v))))

(define (check-optional-function/arity caller desc v . arities)
  (when v
    (unless (procedure? v)
      (contract-error
       "~a: expected ~a to be a function or #f, but got: ~e"
       caller desc v))
    (for ([arity (in-list arities)])
      (unless (procedure-arity-includes? v arity)
        (contract-error
         (string-append
          "~a: expected ~a to be a function that accepts ~a arguments,"
          " but got: ~e")
         caller desc arity v)))))

(define (contract-error fmt . args)
  (raise
   (make-exn:fail:contract
    (apply format fmt args)
    (current-continuation-marks))))

(define-values (prop:dict dict-struct? dict-struct-ref)
  (make-struct-type-property 'dict dict-property-guard))

(define (get-dict-ref v)
  (vector-ref v 0))
(define (get-dict-set! v)
  (vector-ref v 1))
(define (get-dict-set v)
  (vector-ref v 2))
(define (get-dict-remove! v)
  (vector-ref v 3))
(define (get-dict-remove v)
  (vector-ref v 4))
(define (get-dict-count v)
  (vector-ref v 5))
(define (get-dict-iterate-first v)
  (vector-ref v 6))
(define (get-dict-iterate-next v)
  (vector-ref v 7))
(define (get-dict-iterate-key v)
  (vector-ref v 8))
(define (get-dict-iterate-value v)
  (vector-ref v 9))

(define (assoc? v) 
  (and (list? v) (andmap pair? v)))

(define (dict? v)
  (or (hash? v)
      (vector? v)
      (assoc? v)
      (dict-struct? v)))

(define (dict-mutable? d)
  (if (dict? d)
      (or (and (or (hash? d)
                   (vector? d))
               (not (immutable? d)))
          (and (dict-struct? d)
               (get-dict-set! (dict-struct-ref d))
               #t))
      (raise-type-error 'dict-mutable? "dict" d)))

(define (dict-can-remove-keys? d)
  (if (dict? d)
      (or (hash? d)
          (assoc? d)
          (and (dict-struct? d)
               (or (get-dict-remove! (dict-struct-ref d))
                   (get-dict-remove (dict-struct-ref d)))
               #t))
      (raise-type-error 'dict-can-remove-keys? "dict" d)))

(define (dict-can-functional-set? d)
  (if (dict? d)
      (or (and (hash? d) (immutable? d))
          (assoc? d)
          (and (dict-struct? d)
               (get-dict-set (dict-struct-ref d))
               #t))
      (raise-type-error 'dict-can-functional-set? "dict" d)))

(define (dict-has-key? d k)
  (define not-there (gensym))
  (not (eq? not-there (dict-ref d k not-there))))

(define dict-ref
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
     [(dict-struct? d)
      ((get-dict-ref (dict-struct-ref d)) d key)]
     [else
      (raise-type-error 'dict-ref "dict" 0 d key)])]
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
     [(dict-struct? d)
      ((get-dict-ref (dict-struct-ref d)) d key default)]
     [else
      (raise-type-error 'dict-ref "dict" 0 d key default)])]))

(define (dict-ref! d key new)
  (define not-there (gensym))
  (define v (dict-ref d key not-there))
  (if (eq? not-there v)
      (let ([n (if (procedure? new) (new) new)])
        (dict-set! d key n)
        n)
      v))

(define (dict-set! d key val)
  (cond
   [(hash? d) (hash-set! d key val)]
   [(vector? d) (vector-set! d key val)]
   [(assoc? d)
    (raise-type-error 'dict-set! "mutable dict" 0 d key val)]
   [(dict-struct? d)
    (let ([s! (get-dict-set! (dict-struct-ref d))])
      (if s!
          (s! d key val)
          (raise-type-error 'dict-set! "mutable dict" 0 d key val)))]
   [else
    (raise-type-error 'dict-set! "dict" 0 d key val)]))

(define (dict-set*! d . pairs)
  (unless (even? (length pairs))
    (error 'dict-set*! "expected an even number of association elements, but received an odd number: ~e" pairs))
  (let loop ([pairs pairs])
    (unless (null? pairs)
      (dict-set! d (car pairs) (cadr pairs))
      (loop (cddr pairs)))))

(define (dict-set d key val)
  (cond
   [(hash? d) (hash-set d key val)]
   [(vector? d)
    (raise-type-error 'dict-set "functional-update dict" 0  d key val)]
   [(assoc? d)
    (let loop ([xd d])
      (cond
       [(null? xd) (list (cons key val))]
       [else
        (let ([a (car xd)])
          (if (equal? (car a) key) 
              (cons (cons key val) (cdr xd))
              (cons a (loop (cdr xd)))))]))]
   [(dict-struct? d)
    (let ([s (get-dict-set (dict-struct-ref d))])
      (if s
          (s d key val)
          (raise-type-error 'dict-set "functional-update dict" 0 d key val)))]
   [else
    (raise-type-error 'dict-set "dict" 0 d key val)]))

(define (dict-set* d . pairs)
    (unless (even? (length pairs))
      (error 'dict-set* "expected an even number of association elements, but received an odd number: ~e" pairs))
    (let loop ([d d]
               [pairs pairs])
      (if (null? pairs)
          d
          (loop (dict-set d (car pairs) (cadr pairs))
                (cddr pairs)))))

(define dict-update!
  (case-lambda
   [(d key xform)
    (dict-set! d key (xform (dict-ref d key)))]
   [(d key xform default)
    (dict-set! d key (xform (dict-ref d key default)))]))

(define dict-update
  (case-lambda
   [(d key xform)
    (dict-set d key (xform (dict-ref d key)))]
   [(d key xform default)
    (dict-set d key (xform (dict-ref d key default)))]))

(define (dict-remove! d key)
  (cond
   [(hash? d) (hash-remove! d key)]
   [(vector? d)
    (raise-type-error 'dict-remove! "dict with removeable keys" 0 d key)]
   [(assoc? d)
    (raise-type-error 'dict-remove! "mutable dict" 0 d key)]
   [(dict-struct? d)
    (let ([r! (get-dict-remove! (dict-struct-ref d))])
      (if r!
          (r! d key)
          (raise-type-error 'dict-remove! "mutable dict with removable keys" 0 d key)))]
   [else
    (raise-type-error 'dict-remove! "dict" 0 d key)]))

(define (dict-remove d key)
  (cond
   [(hash? d) (hash-remove d key)]
   [(vector? d)
    (raise-type-error 'dict-remove "dict with removeable keys" 0 d key)]
   [(assoc? d)
    (let loop ([xd d])
      (cond
       [(null? xd) null]
       [else
        (let ([a (car xd)])
          (if (equal? (car a) key) 
              (cdr xd)
              (cons a (loop (cdr xd)))))]))]
   [(dict-struct? d)
    (let ([s (get-dict-remove (dict-struct-ref d))])
      (if s
          (s d key)
          (raise-type-error 'dict-remove "dict with functionally removeable keys" 0 d key)))]
   [else
    (raise-type-error 'dict-remove "dict" 0 d key)]))

(define (dict-count d)
  (cond
   [(hash? d) (hash-count d)]
   [(vector? d) (vector-length d)]
   [(assoc? d) (length d)]
   [(dict-struct? d) ((get-dict-count (dict-struct-ref d)) d)]
   [else
    (raise-type-error 'dict-count "dict" d)]))

(struct assoc-iter (head pos))

(define (dict-iterate-first d)
  (cond
   [(hash? d) (hash-iterate-first d)]
   [(vector? d) (if (zero? (vector-length d))
                    #f
                    0)]
   [(assoc? d) (if (null? d) #f (assoc-iter d d))]
   [(dict-struct? d) ((get-dict-iterate-first (dict-struct-ref d)) d)]
   [else
    (raise-type-error 'dict-iterate-first "dict" d)]))

(define (dict-iterate-next d i)
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
   [(dict-struct? d) ((get-dict-iterate-next (dict-struct-ref d)) d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-next
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-type-error 'dict-iterate-next "dict" d)]))

(define (dict-iterate-key d i)
  (cond
   [(hash? d) (hash-iterate-key d i)]
   [(vector? d) i]
   [(and (assoc-iter? i) (eq? d (assoc-iter-head i))) (caar (assoc-iter-pos i))]
   [(dict-struct? d) ((get-dict-iterate-key (dict-struct-ref d)) d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-key
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-type-error 'dict-iterate-key "dict" d)]))

(define (dict-iterate-value d i)
  (cond
   [(hash? d) (hash-iterate-value d i)]
   [(vector? d) (vector-ref d i)]
   [(and (assoc-iter? i) (eq? d (assoc-iter-head i))) (cdar (assoc-iter-pos i))]
   [(dict-struct? d) ((get-dict-iterate-value (dict-struct-ref d)) d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-value
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-type-error 'dict-iterate-value "dict" d)]))

(define-sequence-syntax :in-dict
  (lambda () #'in-dict)
  (lambda (stx)
    (syntax-case stx ()
      [((key-id val-id) (_ dict-expr))
       #'[(key-id val-id)
          (:do-in ([(d) dict-expr])
                  (void)
                  ([i (dict-iterate-first d)])
                  i
                  ([key-id (dict-iterate-key d i)]
                   [val-id (dict-iterate-value d i)])
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
  #:property prop:dict
  (vector custom-hash-ref
          custom-hash-set!
          #f
          custom-hash-remove!
          #f
          custom-hash-count
          custom-hash-iterate-first
          custom-hash-iterate-next
          custom-hash-iterate-key
          custom-hash-iterate-value)
  #:property prop:equal+hash
  (list (lambda (a b recur)
          (and (recur (custom-hash-make-box a)
                      (custom-hash-make-box b))
               (recur (custom-hash-table a)
                      (custom-hash-table b))))
        (lambda (a recur) (recur (custom-hash-table a)))
        (lambda (a recur) (recur (custom-hash-table a)))))

(struct immutable-custom-hash custom-hash ()
  #:property prop:dict
  (vector custom-hash-ref
          #f
          custom-hash-set
          #f
          custom-hash-remove
          custom-hash-count
          custom-hash-iterate-first
          custom-hash-iterate-next
          custom-hash-iterate-key
          custom-hash-iterate-value))
          
(define-values (create-custom-hash 
                create-immutable-custom-hash
                make-weak-custom-hash)
  (let ([mk
         (lambda (hash hash2 =? who make-custom-hash table wrap-make-box)
           (unless (and (procedure? =?)
                        (procedure-arity-includes? =? 2))
             (raise-type-error who "procedure (arity 2)" =?))
           (unless (and (procedure? hash)
                        (procedure-arity-includes? hash 1))
             (raise-type-error who "procedure (arity 1)" hash))
           (unless (and (procedure? hash2)
                        (procedure-arity-includes? hash2 1))
             (raise-type-error who "procedure (arity 1)" hash2))
           (let ()
             (struct box hash-box ()
               #:property prop:equal+hash (list
                                           (lambda (a b recur) 
                                             (=? (hash-box-key a) (hash-box-key b)))
                                           (lambda (v recur) 
                                             (hash (hash-box-key v)))
                                           (lambda (v recur)
                                             (hash2 (hash-box-key v)))))
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

(provide prop:dict
         dict?
         dict-mutable?
         dict-can-remove-keys?
         dict-can-functional-set?
         dict-has-key?
         dict-ref
         dict-ref!
         dict-set!
         dict-set
         dict-set*!
         dict-set*
         dict-update!
         dict-update
         dict-remove!
         dict-remove
         dict-count
         dict-iterate-first
         dict-iterate-next
         dict-iterate-key
         dict-iterate-value
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
  
