#lang racket/base

(require (for-syntax racket/base))

(provide prop:dict
         dict?

         dict-mutable?
         dict-can-remove-keys?
         dict-can-functional-set?
         
         dict-ref
         dict-set!
         dict-set
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

         in-dict
         in-dict-keys
         in-dict-values
         in-dict-pairs

         (rename-out [create-custom-hash make-custom-hash]
                     [create-immutable-custom-hash make-immutable-custom-hash])
         make-weak-custom-hash)

(define-values (prop:dict dict-struct? dict-struct-ref)
  (make-struct-type-property 'dict
                             (lambda (v info)
                               (unless (and 
                                        (vector? v)
                                        (= 10 (vector-length v))
                                        (let-values ([(ref set! set remove! remove count
                                                           iterate-first iterate-next
                                                           iterate-key iterate-value)
                                                      (vector->values v)])
                                          (and (procedure? ref)
                                               (and (procedure-arity-includes? ref 2)
                                                    (procedure-arity-includes? ref 3))
                                               (or (not set!)
                                                   (and (procedure? set!)
                                                        (procedure-arity-includes? set! 3)))
                                               (or (not set)
                                                   (and (procedure? set)
                                                        (procedure-arity-includes? set 3)))
                                               (or (not remove!)
                                                   (and (procedure? remove!)
                                                        (procedure-arity-includes? remove! 2)))
                                               (or (not remove)
                                                   (and (procedure? remove)
                                                        (procedure-arity-includes? remove 2)))
                                               (procedure? count)
                                               (procedure-arity-includes? count 1)
                                               (procedure? iterate-first)
                                               (procedure-arity-includes? iterate-first 1)
                                               (procedure? iterate-next)
                                               (procedure-arity-includes? iterate-next 2)
                                               (procedure? iterate-key)
                                               (procedure-arity-includes? iterate-key 2)
                                               (procedure? iterate-value)
                                               (procedure-arity-includes? iterate-value 2))))
                                 (raise-type-error 'prop:dict-guard
                                                   "vector of dict methods"
                                                   v))
                               v)))

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
