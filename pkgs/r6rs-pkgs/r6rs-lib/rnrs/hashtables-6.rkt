#lang scheme/base

(require scheme/dict)

(provide make-eq-hashtable
         make-eqv-hashtable
         (rename-out [r6rs:make-hashtable make-hashtable])
         hashtable?
         hashtable-size
         hashtable-ref
         hashtable-set!
         hashtable-delete!
         hashtable-contains?
         hashtable-update!
         hashtable-copy
         hashtable-clear!
         hashtable-keys
         hashtable-entries
         hashtable-equivalence-function
         hashtable-hash-function
         hashtable-mutable?
         equal-hash
         string-hash
         string-ci-hash
         symbol-hash)

(define-struct hashtable ([ht #:mutable] 
                          wrap
                          unwrap
                          mutable? 
                          equivalence-function
                          hash-function)
  #:property prop:dict
  (vector (case-lambda
           [(ht key)
            (hash-ref (hashtable-ht ht) ((hashtable-wrap ht) key))]
           [(ht key default)
            (hash-ref (hashtable-ht ht) ((hashtable-wrap ht) key) default)])
          (lambda (ht key val) (hashtable-set! ht key val))
          #f
          (lambda (ht key) (hashtable-delete! ht key))
          #f
          (lambda (ht) (hashtable-size ht))
          (lambda (ht) (hash-iterate-first (hashtable-ht ht)))
          (lambda (ht pos) (hash-iterate-next (hashtable-ht ht) pos))
          (lambda (ht pos) ((hashtable-unwrap ht)
                            (hash-iterate-key (hashtable-ht ht) pos)))
          (lambda (ht pos) (hash-iterate-value (hashtable-ht ht) pos))))

(define-struct eqv-box (val)
  #:property prop:equal+hash (list
                              (lambda (a b recur) (eqv? (eqv-box-val a)
                                                        (eqv-box-val b)))
                              (lambda (v recur) (equal-hash-code (eqv-box-val v)))
                              (lambda (v recur) (equal-secondary-hash-code (eqv-box-val v)))))
                              

(define (make-eq-hashtable [k 0])
  (unless (exact-nonnegative-integer? k)
    (raise-type-error 'make-eq-hashtable "exact, nonnegative integer" k))
  (make-hashtable (make-hasheq) values values #t eq? #f))

(define (make-eqv-hashtable [k 0])
  (unless (exact-nonnegative-integer? k)
    (raise-type-error 'make-eqv-hashtable "exact, nonnegative integer" k))
  (make-hashtable (make-hash) make-eqv-box eqv-box-val #t eqv? #f))

(define r6rs:make-hashtable
  (let ([make-hashtable
         (lambda (hash =? [k 0])
           (unless (and (procedure? hash)
                        (procedure-arity-includes? hash 1))
             (raise-type-error 'make-hashtable "procedure (arity 1)" hash))
           (unless (and (procedure? =?)
                        (procedure-arity-includes? =? 2))
             (raise-type-error 'make-hashtable "procedure (arity 2)" =?))
           (unless (exact-nonnegative-integer? k)
             (raise-type-error 'make-hashtable "exact, nonnegative integer" k))
           (let ()
             (define-struct hash-box (val)
               #:property prop:equal+hash (list
                                           (lambda (a b recur) (=? (hash-box-val a)
                                                                   (hash-box-val b)))
                                           (lambda (v recur) (hash (hash-box-val v)))
                                           (lambda (v recur) 10001)))
             (make-hashtable (make-hash) make-hash-box hash-box-val #t =? hash)))])
    make-hashtable))

(define (hashtable-size ht)
  (hash-count (hashtable-ht ht)))

(define tag (gensym))

(define (hashtable-ref ht key default)
  (let ([v (hash-ref (hashtable-ht ht) ((hashtable-wrap ht) key) tag)])
    (if (eq? v tag)
        default
        v)))

(define (hashtable-set! ht key val)
  (if (hashtable-mutable? ht)
      (hash-set! (hashtable-ht ht) ((hashtable-wrap ht) key) val)
      (raise-type-error 'hashtable-set! "mutable hashtable" ht)))

(define (hashtable-delete! ht key)
  (if (hashtable-mutable? ht)
      (hash-remove! (hashtable-ht ht) ((hashtable-wrap ht) key))
      (raise-type-error 'hashtable-delete! "mutable hashtable" ht)))

(define (hashtable-contains? ht key)
  (not (eq? (hash-ref (hashtable-ht ht) ((hashtable-wrap ht) key) tag)
            tag)))

(define (hashtable-update! ht key proc default)
  (if (hashtable-mutable? ht)
      (hashtable-set! ht key (proc (hashtable-ref ht key default)))
      (raise-type-error 'hashtable-update! "mutable hashtable" ht)))

(define (hashtable-copy ht [mutable? #f])
  (make-hashtable (hash-copy (hashtable-ht ht))
                  (hashtable-wrap ht)
                  (hashtable-unwrap ht)
                  mutable?
                  (hashtable-equivalence-function ht)
                  (hashtable-hash-function ht)))

(define (hashtable-clear! ht [k 0])
  (unless (exact-nonnegative-integer? k)
    (raise-type-error 'hashtable-clear! "exact, nonnegative integer" k))
  (if (hashtable-mutable? ht)
      (set-hashtable-ht! ht (if (eq? values (hashtable-wrap ht))
                                (make-hasheq)
                                (make-hash)))
      (raise-type-error 'hashtable-clear! "mutable hashtable" ht)))

(define (hashtable-keys ht)
  (let ([unwrap (hashtable-unwrap ht)])
    (list->vector (hash-map (hashtable-ht ht) (lambda (a b) (unwrap a))))))

(define (hashtable-entries ht)
  (let ([ps (hash-map (hashtable-ht ht) cons)]
        [unwrap (hashtable-unwrap ht)])
    (values (list->vector (map (lambda (p) (unwrap (car p))) ps))
            (list->vector (map cdr ps)))))

(define (equal-hash v)
  (abs (equal-hash-code v)))

(define (string-hash s)
  (unless (string? s)
    (raise-type-error 'string-hash "string" s))
  (abs (equal-hash-code s)))

(define (string-ci-hash s)
  (unless (string? s)
    (raise-type-error 'string-ci-hash "string" s))
  (abs (equal-hash-code (string-foldcase s))))

(define (symbol-hash s)
  (unless (symbol? s)
    (raise-type-error 'symbol-hash "symbol" s))
  (abs (eq-hash-code s)))
