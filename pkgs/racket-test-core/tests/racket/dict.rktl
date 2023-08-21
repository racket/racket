
(load-relative "loadtest.rktl")

(Section 'dict)

(require racket/dict racket/generic)

;; Currently relying on the `map' an `for-each' to test `dict-iterate-...',
;; and custom hashes to test `prop:dict' use.

(define (try-simple d ordered? mutable? can-remove? can-update? [orig-one 1])

  ;; Assuming that dictionaries with nondeterministic order, e.g. hash tables,
  ;; will at least have some internal order to follow, and will only differ in
  ;; whether they proceed left-to-right or right-to-left for each function.
  (define test/order
    (if ordered?
        test
        (lambda (expected name actual)
          (let ([rev (reverse expected)])
            (if (equal? rev actual)
                (test rev name actual)
                (test expected name actual))))))

  (test #t dict? d)

  (test 'one dict-ref d 1)
  (test #t dict-has-key? d 1)
  (test 'nope dict-ref d 100 'nope)
  (test #f dict-has-key? d 100)
  (test 'nope dict-ref d 100 (lambda () 'nope))
  
  (test #t ormap values (dict-map d (lambda (k v) (equal? k orig-one))))
  (test #t ormap values (dict-map d (lambda (k v) (equal? v 'one))))
  (test #f ormap values (dict-map d (lambda (k v) (equal? k 100))))

  (test mutable? dict-mutable? d)
  (test can-remove? dict-can-remove-keys? d)
  (test can-update? dict-can-functional-set? d)

  (test/order (dict-map d cons) 'dict->list (dict->list d))
  (test/order (dict-map d (λ (k v) k)) 'dict-keys (dict-keys d))
  (test/order (dict-map d (λ (k v) v)) 'dict-values (dict-values d))
  
  (test/order (dict-map d cons) 'in-dict
              (for/list ([(k v) (in-dict d)])
                (cons k v)))
  (test/order (dict-map d cons) 'in-dict/keys/vals
              (for/list ([k (in-dict-keys d)]
                         [v (in-dict-values d)])
                (cons k v)))
  (test/order (dict-map d cons) 'in-dict-pairs
              (for/list ([p (in-dict-pairs d)])
                p))
  
  (let ([l null])
    (dict-for-each d (lambda (k v) (set! l (cons (cons k v) l))))
    (test/order (reverse l) 'dict-for-each/map (dict-map d cons))
    (test (length l) dict-count d))

  (if (not can-remove?)
      (begin
        (err/rt-test (dict-remove! d 1))
        (err/rt-test (dict-remove d 1))
        (err/rt-test (dict-set d 1 "ONE"))
        (test (void) dict-set! d 1 "ONE")
        (test "ONE" dict-ref d 1)
        (test (void) dict-set*! d 1 (gensym) 1 "TWO")
        (err/rt-test (dict-set*! d 1) exn:fail?)
        (test "TWO" dict-ref d 1)
        (test "TWO" dict-ref! d 1 (gensym)))
      (let ([cnt (dict-count d)]
            [smaller (if mutable?
                         (begin
                           (err/rt-test (dict-remove d 1))
                           (dict-remove! d 1)
                           d)
                         (begin
                           (err/rt-test (dict-remove! d 1))
                           (dict-remove d 1)))])
        (test 'nope dict-ref smaller 1 'nope)
        (test (sub1 cnt) dict-count smaller)
        (let ([try-add
               (lambda (d val)
                 (let ([bigger (if mutable?
                                   (begin
                                     (err/rt-test (dict-set smaller 1 val))
                                     (dict-set! smaller 1 val)
                                     d)
                                   (begin
                                     (err/rt-test (dict-set! smaller 1 val))
                                     (dict-set smaller 1 val)))])
                   (test cnt dict-count bigger)
                   (when (eq? val 'one)
                     (unless (pair? d)
                       (test #t equal? d bigger)))))])
          (try-add smaller "ONE")
          (try-add d "ONE")
          (try-add d 'one))
        (let ([try-add
               (lambda (d val)
                 (let ([bigger (if mutable?
                                   (begin
                                     (err/rt-test (dict-set* smaller 1 val))
                                     (dict-set*! smaller 1 (gensym) 1 val)
                                     (err/rt-test (dict-set*! smaller 1) exn:fail?)
                                     d)
                                   (begin
                                     (err/rt-test (dict-set*! smaller 1 val))
                                     (err/rt-test (dict-set* smaller 1) exn:fail?)
                                     (dict-set* smaller 1 (gensym) 1 val)))])
                   (test cnt dict-count bigger)
                   (when (eq? val 'one)
                     (unless (pair? d)
                       (test #t equal? d bigger)))))])
          (try-add smaller "ONE")
          (try-add d "ONE")
          (try-add d 'one)))))

(define (dict-set/list d kvs)
  (for/fold ([acc d]) ([kv (in-list kvs)])
    (dict-set acc (car kv) (cdr kv))))

(define (dict-set!/list d kvs)
  (for ([kv (in-list kvs)])
    (dict-set! d (car kv) (cdr kv))))

(define (dict-new d kvs)
  (cond
    [(dict-can-functional-set? d)
     (dict-set/list (dict-clear d) kvs)]
    [(dict-mutable? d)
     (define dn (dict-copy d))
     (dict-clear! dn)
     (dict-set!/list dn kvs)
     dn]
    [else
     (error 'dict-new "expected a functional or mutable dict, given ~v" d)]))

(define (dict=? a b)
  (and (for/and ([(k av) (in-dict a)])
         (and (dict-has-key? b k) (equal? av (dict-ref b k))))
       (for/and ([(k bv) (in-dict b)])
         (and (dict-has-key? a k) (equal? (dict-ref a k) bv)))))

(define (test-dict-map/copy d d?)
  (define (make-dict kvs) (dict-new d kvs))
  (test "apple" dict-ref (make-dict '((a . "apple") (b . "banana"))) 'a)
  (test "banana" dict-ref (make-dict '((a . "apple") (b . "banana"))) 'b)
  (test "go fish" dict-ref (make-dict '((a . "apple") (b . "banana"))) 'c "go fish")
  (test #t d? (make-dict '()))
  (test #t d? (make-dict '((a . "apple") (b . "banana"))))
  (test/compare dict=? (make-dict '()) make-dict '())
  (test/compare dict=?
                (make-dict '((a . "apple") (b . "banana")))
                make-dict
                '((b . "banana") (a . "apple")))
  (test/compare dict=?
                '((a . "apple") (b . "banana"))
                dict->list
                (make-dict '((a . "apple") (b . "banana"))))
  (test/compare dict=?
                '((a . "APPLE") (b . "BANANA"))
                dict-map
                (make-dict '((a . "apple") (b . "banana")))
                (λ (k v)
                  (cons k (string-upcase v))))
  (test/compare dict=?
                '((a . "APPLE") (b . "BANANA"))
                dict-map
                (make-dict '((a . "apple") (b . "banana")))
                (λ (k v)
                  (cons k (string-upcase v))))
  (test #t
        d?
        (dict-map/copy
         (make-dict '((a . "apple") (b . "banana")))
         (λ (k v)
           (values k (string-upcase v)))))
  (test/compare dict=?
                (make-dict '((a . "APPLE") (b . "BANANA")))
                dict-map/copy
                (make-dict '((a . "apple") (b . "banana")))
                (λ (k v)
                  (values k (string-upcase v)))))

;; ---------------------------------------------------------

(try-simple (vector 'zero 'one 'two) #t #t #f #f)
(try-simple #hash((1 . one) (#f . 7)) #f #f #t #t)

(let ([d (make-hasheq '((1 . one) (#f . 7)))])
  (test 'one dict-ref! d 1 (gensym))
  (test 'two dict-ref! d 2 'two)
  (test 'two dict-ref d 2)
  (test 'three dict-ref! d 3 (λ () 'three))
  (test 'three dict-ref d 3))

(try-simple #hasheq((1 . one) (#f . 7)) #f #f #t #t)
(try-simple (hash-copy #hash((1 . one) (#f . 7))) #f #t #t #f)
(try-simple (hash-copy #hasheq((1 . one) (#f . 7))) #f #t #t #f)
(try-simple '((0 . zero) (1 . one)) #t #f #t #t)
(try-simple '((1 . one) (0 . zero)) #t #f #t #t)

(test-dict-map/copy '() (listof pair?))
(test-dict-map/copy (hash) (and/c hash? hash-equal? immutable?))
(test-dict-map/copy (hasheqv) (and/c hash? hash-eqv? immutable?))
(test-dict-map/copy (hasheq) (and/c hash? hash-eq? immutable?))
(test-dict-map/copy (make-hash)
                      (and/c hash? hash-equal? (not/c immutable?)))
(test-dict-map/copy (make-hasheqv)
                      (and/c hash? hash-eqv? (not/c immutable?)))
(test-dict-map/copy (make-hasheq)
                      (and/c hash? hash-eq? (not/c immutable?)))
(test-dict-map/copy (make-weak-hash)
                      (and/c hash? hash-equal? hash-weak?))
(test-dict-map/copy (make-weak-hasheqv)
                      (and/c hash? hash-eqv? hash-weak?))
(test-dict-map/copy (make-weak-hasheq)
                      (and/c hash? hash-eq? hash-weak?))
(test-dict-map/copy (make-ephemeron-hash)
                      (and/c hash? hash-equal? hash-ephemeron?))
(test-dict-map/copy (make-ephemeron-hasheqv)
                      (and/c hash? hash-eqv? hash-ephemeron?))
(test-dict-map/copy (make-ephemeron-hasheq)
                      (and/c hash? hash-eq? hash-ephemeron?))

(let ()
  (define (key? x) #t)
  (define (key-code a rec) (rec (format "~a" a)))
  (define (key=? x y rec) (rec (format "~a" x) (format "~a" y)))
  (define-custom-hash-types string-hash #:key? key? key=? key-code)

  (test-dict-map/copy (make-mutable-string-hash) mutable-string-hash?)
  (test-dict-map/copy (make-immutable-string-hash) immutable-string-hash?)
  
  (try-simple (let ([h (make-mutable-string-hash)])
                (dict-set! h "1" 'one)
                (dict-set! h "2" 'two)
                h)
              #f #t #t #f
              "1")
  (try-simple (let* ([h (make-immutable-string-hash)]
                     [h (dict-set h "1" 'one)]
                     [h (dict-set h "2" 'two)])
                h)
              #f #f #t #t
              "1")
  (let ([s1 (make-string 1 #\1)]
        [s2 (make-string 1 #\2)])
    (try-simple (let ([h (make-weak-string-hash)])
                  (dict-set! h s1 'one)
                  (dict-set! h s2 'two)
                  (collect-garbage)
                  h)
                #f #t #t #f
                "1")
    ;; preserve from GC:
    (list s1 s2)))

;; Check behavior on a list of pairs that isn't
;; a dictionary due to duplicate keys:
(test 1 dict-ref '((a . 1) (b . 2) (a . 3)) 'a)
(test '((b . 2) (a . 3)) dict-remove '((a . 1) (b . 2) (a . 3)) 'a)
(test '((a . 4) (b . 2) (a . 3)) dict-set '((a . 1) (b . 2) (a . 3)) 'a 4)
(test 3 dict-count '((a . 1) (b . 2) (a . 3)))
(test '((a 1) (b 2) (a 3)) dict-map '((a . 1) (b . 2) (a . 3)) list)
(test '(a b a) dict-keys '((a . 1) (b . 2) (a . 3)))
(test '(1 2 3) dict-values '((a . 1) (b . 2) (a . 3)))

(let ()
  ;; immutable wrapping immutable
  (define-struct idict (inside)
    #:methods gen:dict
    [(define/generic inside-ref dict-ref)
     (define/generic inside-set dict-set)
     (define/generic inside-clear dict-clear)
     (define/generic inside-iterate-first dict-iterate-first)
     (define/generic inside-iterate-next dict-iterate-next)
     (define/generic inside-iterate-key dict-iterate-key)
     (define/generic inside-iterate-value dict-iterate-value)
     (define (dict-ref dict key
                       [default (lambda () (error "key not found" key))])
       (inside-ref (idict-inside dict) key default))
     (define (dict-set dict key val)
       (idict (inside-set (idict-inside dict) key val)))
     (define (dict-clear dict)
       (idict (inside-clear (idict-inside dict))))
     (define (dict-iterate-first dict)
       (inside-iterate-first (idict-inside dict)))
     (define (dict-iterate-next dict pos)
       (inside-iterate-next (idict-inside dict) pos))
     (define (dict-iterate-key dict pos)
       (inside-iterate-key (idict-inside dict) pos))
     (define (dict-iterate-value dict pos)
       (inside-iterate-value (idict-inside dict) pos))])

  ;; mutable wrapping mutable
  (define-struct mdict (inside)
    #:methods gen:dict
    [(define/generic inside-ref dict-ref)
     (define/generic inside-set! dict-set!)
     (define/generic inside-copy dict-copy)
     (define/generic inside-remove! dict-remove!)
     (define/generic inside-iterate-first dict-iterate-first)
     (define/generic inside-iterate-next dict-iterate-next)
     (define/generic inside-iterate-key dict-iterate-key)
     (define/generic inside-iterate-value dict-iterate-value)
     (define (dict-ref dict key
                       [default (lambda () (error "key not found" key))])
       (inside-ref (mdict-inside dict) key default))
     (define (dict-set! dict key val)
       (inside-set! (mdict-inside dict) key val))
     (define (dict-copy dict)
       (mdict (inside-copy (mdict-inside dict))))
     (define (dict-remove! dict key)
       (inside-remove! (mdict-inside dict) key))
     (define (dict-iterate-first dict)
       (inside-iterate-first (mdict-inside dict)))
     (define (dict-iterate-next dict pos)
       (inside-iterate-next (mdict-inside dict) pos))
     (define (dict-iterate-key dict pos)
       (inside-iterate-key (mdict-inside dict) pos))
     (define (dict-iterate-value dict pos)
       (inside-iterate-value (mdict-inside dict) pos))])

  ;; mutable wrapping immutable
  (define idict-set dict-set)
  (define idict-remove dict-remove)
  (define-struct mdicti (inside) #:mutable
    #:methods gen:dict
    [(define/generic inside-ref dict-ref)
     (define/generic inside-iterate-first dict-iterate-first)
     (define/generic inside-iterate-next dict-iterate-next)
     (define/generic inside-iterate-key dict-iterate-key)
     (define/generic inside-iterate-value dict-iterate-value)
     (define (dict-ref dict key
                       [default (lambda () (error "key not found" key))])
       (inside-ref (mdicti-inside dict) key default))
     (define (dict-set! dict key val)
       (set-mdicti-inside! dict (idict-set (mdicti-inside dict) key val)))
     (define (dict-copy dict)
       (mdicti (mdicti-inside dict)))
     (define (dict-remove! dict key)
       (set-mdicti-inside! dict (idict-remove (mdicti-inside dict) key)))
     (define (dict-iterate-first dict)
       (inside-iterate-first (mdicti-inside dict)))
     (define (dict-iterate-next dict pos)
       (inside-iterate-next (mdicti-inside dict) pos))
     (define (dict-iterate-key dict pos)
       (inside-iterate-key (mdicti-inside dict) pos))
     (define (dict-iterate-value dict pos)
       (inside-iterate-value (mdicti-inside dict) pos))])

  ;; immutable wrapping mutable by copying state
  (define mdict-set! dict-set!)
  (define mdict-copy dict-copy)
  (define mdict-clear! dict-clear!)
  (define-struct idictm (inside)
    #:methods gen:dict
    [(define/generic inside-ref dict-ref)
     (define/generic inside-iterate-first dict-iterate-first)
     (define/generic inside-iterate-next dict-iterate-next)
     (define/generic inside-iterate-key dict-iterate-key)
     (define/generic inside-iterate-value dict-iterate-value)
     (define (dict-ref dict key
                       [default (lambda () (error "key not found" key))])
       (inside-ref (idictm-inside dict) key default))
     (define (dict-set dict key val)
       (define copy (mdict-copy (idictm-inside dict)))
       (mdict-set! copy key val)
       (idictm copy))
     (define (dict-clear dict)
       (define copy (mdict-copy (idictm-inside dict)))
       (mdict-clear! copy)
       (idictm copy))
     (define (dict-iterate-first dict)
       (inside-iterate-first (idictm-inside dict)))
     (define (dict-iterate-next dict pos)
       (inside-iterate-next (idictm-inside dict) pos))
     (define (dict-iterate-key dict pos)
       (inside-iterate-key (idictm-inside dict) pos))
     (define (dict-iterate-value dict pos)
       (inside-iterate-value (idictm-inside dict) pos))])

  (test-dict-map/copy (idict '()) idict?)
  (test-dict-map/copy (mdict (make-hash)) mdict?)
  (test-dict-map/copy (mdicti '()) mdicti?)
  (test-dict-map/copy (idictm (make-hash)) idictm?))

;; ----------------------------------------

(report-errs)
