
(load-relative "loadtest.rktl")

(Section 'hash)

(require racket/hash)

;; ----------------------------------------
;; Hash-key sorting:

(let ([u/apple (string->uninterned-symbol "apple")]
      [u/banana (string->uninterned-symbol "banana")]
      [u/coconut (string->uninterned-symbol "coconut")]
      [apple (string->unreadable-symbol "apple")]
      [banana (string->unreadable-symbol "banana")]
      [coconut (string->unreadable-symbol "coconut")])
  (test (list #f #t
              #\a #\b #\c #\u3BB
              (- (expt 2 79))
              -3 -2 -1
              0
              1/2 0.75 8.5f-1
              1 2 3
              (expt 2 79)
              u/apple u/banana u/coconut
              apple banana coconut
              'apple 'banana 'coconut 'coconut+
              '#:apple '#:banana '#:coconut
              "Apple"
              "apple" "banana" "coconut"
              #"Apple"
              #"apple" #"banana" #"coconut"
              null
              (void)
              eof)
        'ordered
        (hash-map (hash #t 'x
                        #f 'x
                        #\a 'a #\b 'b #\c 'c #\u3BB 'lam
                        1 'a 2 'b 3 'c
                        1/2 'n 0.75 'n 8.5f-1 'n
                        0 'z
                        -1 'a -2 'b -3 'c
                        (expt 2 79) 'b
                        (- (expt 2 79)) 'b
                        "Apple" 'a
                        "apple" 'a "banana" 'b "coconut" 'c
                        #"Apple" 'a
                        #"apple" 'a #"banana" 'b #"coconut" 'c
                        u/apple 'a u/banana 'b u/coconut 'c
                        apple 'a banana 'b coconut 'c
                        'apple 'a 'banana 'b 'coconut 'c 'coconut+ '+
                        '#:apple 'a '#:banana 'b '#:coconut 'c
                        null 'one
                        (void) 'one
                        eof 'one)
                  (lambda (k v) k)
                  #t)))

;; ----------------------------------------

(test #hash([4 . four] [3 . three] [1 . one] [2 . two])
      hash-union #hash([1 . one] [2 . two]) #hash([3 . three] [4 . four]))
(test #hash([four . 4] [three . 3] [one . 1] [two . 2])
      hash-union #hash([one . 1] [two . 1]) #hash([three . 3] [four . 4] [two . 1])
      #:combine +)

(let ()
  (define h (make-hash))
  (hash-union! h #hash([1 . one] [2 . two]))
  (hash-union! h #hash([3 . three] [4 . four]))
  (test #t
        equal?
        (hash-copy
         #hash([1 . one] [2 . two] [3 . three] [4 . four]))
        h))
(let ()
  (define h (make-hash))
  (hash-union! h #hash([one . 1] [two . 1]))
  (err/rt-test (hash-union! h #hash([three . 3] [four . 4] [two . 1])) exn:fail?))
(let ()
  (define h (make-hash))
  (hash-union! h #hash([one . 1] [two . 1]))
  (hash-union! h #hash([three . 3] [four . 4] [two . 1])
               #:combine/key (lambda (k x y) (+ x y)))
  (test #t
        equal?
        (hash-copy
         #hash([one . 1] [two . 2] [three . 3] [four . 4]))
        h))

(let ()
  (struct a (n m)
          #:property
          prop:equal+hash
          (list (lambda (a b eql) (and (= (a-n a) (a-n b))
                                  (= (a-m a) (a-m b))))
                (lambda (a hc) (a-n a))
                (lambda (a hc) (a-n a))))

  (define ht0 (hash (a 1 0) #t))
  ;; A hash table with two keys that have the same hash code
  (define ht1 (hash (a 1 0) #t
                    (a 1 2) #t))
  ;; Another hash table with the same two keys, plus another
  ;; with an extra key whose hash code is different but the
  ;; same in the last 5 bits:
  (define ht2 (hash (a 1 0) #t
                    (a 1 2) #t
                    (a 33 0) #t))
  ;; A hash table with no collision, but the same last
  ;; 5 bits for both keys:
  (define ht3 (hash (a 1 0) #t
                    (a 33 0) #t))

  ;; Subset must compare a collision node with a subtree node (that
  ;; contains a collision node):
  (test #t hash-keys-subset? ht1 ht2)

  (test #t hash-keys-subset? ht3 ht2)
  (test #t hash-keys-subset? ht0 ht3)

  (test #t hash-keys-subset? ht0 ht2)
  (test #t hash-keys-subset? ht0 ht1)
  (test #f hash-keys-subset? ht2 ht1)
  (test #f hash-keys-subset? ht2 ht0)
  (test #f hash-keys-subset? ht1 ht0)
  (test #f hash-keys-subset? ht1 ht3))

(let ()
  (define-syntax (define-hash-iterations-tester stx)
    (syntax-case stx ()
     [(_ tag -in-hash -in-pairs -in-keys -in-values)
      #'(define-hash-iterations-tester tag
          -in-hash -in-hash -in-hash
          -in-pairs -in-pairs -in-pairs
          -in-keys -in-keys -in-keys
          -in-values -in-values -in-values)]
     [(_ tag
         -in-immut-hash -in-mut-hash -in-weak-hash
         -in-immut-hash-pairs -in-mut-hash-pairs -in-weak-hash-pairs
         -in-immut-hash-keys -in-mut-hash-keys -in-weak-hash-keys
         -in-immut-hash-values -in-mut-hash-values -in-weak-hash-values)
      (with-syntax 
       ([name 
         (datum->syntax #'tag 
           (string->symbol 
             (format "test-hash-iters-~a" (syntax->datum #'tag))))])
       #'(define (name lst1 lst2)
          (define ht/immut (make-immutable-hash (map cons lst1 lst2)))
          (define ht/mut (make-hash (map cons lst1 lst2)))
          (define ht/weak (make-weak-hash (map cons lst1 lst2)))
            
          (define fake-ht/immut
            (chaperone-hash 
                ht/immut
              (lambda (h k) (values k (lambda (h k v) v))) ; ref-proc
              (lambda (h k v) values k v) ; set-proc
              (lambda (h k) k) ; remove-proc
              (lambda (h k) k))) ; key-proc
          (define fake-ht/mut
            (impersonate-hash 
                ht/mut
              (lambda (h k) (values k (lambda (h k v) v))) ; ref-proc
              (lambda (h k v) values k v) ; set-proc
              (lambda (h k) k) ; remove-proc
              (lambda (h k) k))) ; key-proc
          (define fake-ht/weak
            (impersonate-hash 
                ht/weak
              (lambda (h k) (values k (lambda (h k v) v))) ; ref-proc
              (lambda (h k v) values k v) ; set-proc
              (lambda (h k) k) ; remove-proc
              (lambda (h k) k))) ; key-proc
            
          (define ht/immut/seq (-in-immut-hash ht/immut))
          (define ht/mut/seq (-in-mut-hash ht/mut))
          (define ht/weak/seq (-in-weak-hash ht/weak))
          (define ht/immut-pair/seq (-in-immut-hash-pairs ht/immut))
          (define ht/mut-pair/seq (-in-mut-hash-pairs ht/mut))
          (define ht/weak-pair/seq (-in-weak-hash-pairs ht/weak))
          (define ht/immut-keys/seq (-in-immut-hash-keys ht/immut))
          (define ht/mut-keys/seq (-in-mut-hash-keys ht/mut))
          (define ht/weak-keys/seq (-in-weak-hash-keys ht/weak))
          (define ht/immut-vals/seq (-in-immut-hash-values ht/immut))
          (define ht/mut-vals/seq (-in-mut-hash-values ht/mut))
          (define ht/weak-vals/seq (-in-weak-hash-values ht/weak))
    
          (test #t =
           (for/sum ([(k v) (-in-immut-hash ht/immut)]) (+ k v))
           (for/sum ([(k v) (-in-mut-hash ht/mut)]) (+ k v))
           (for/sum ([(k v) (-in-weak-hash ht/weak)]) (+ k v))
           (for/sum ([(k v) (-in-immut-hash fake-ht/immut)]) (+ k v))
           (for/sum ([(k v) (-in-mut-hash fake-ht/mut)]) (+ k v))
           (for/sum ([(k v) (-in-weak-hash fake-ht/weak)]) (+ k v))
           (for/sum ([(k v) ht/immut/seq]) (+ k v))
           (for/sum ([(k v) ht/mut/seq]) (+ k v))
           (for/sum ([(k v) ht/weak/seq]) (+ k v))
           (for/sum ([k+v (-in-immut-hash-pairs ht/immut)])
             (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v (-in-mut-hash-pairs ht/mut)])
             (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v (-in-weak-hash-pairs ht/weak)])
             (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v (-in-immut-hash-pairs fake-ht/immut)]) 
             (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v (-in-mut-hash-pairs fake-ht/mut)]) 
             (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v (-in-weak-hash-pairs fake-ht/weak)]) 
             (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v ht/immut-pair/seq]) (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v ht/mut-pair/seq]) (+ (car k+v) (cdr k+v)))
           (for/sum ([k+v ht/weak-pair/seq]) (+ (car k+v) (cdr k+v)))
           (+ (for/sum ([k (-in-immut-hash-keys ht/immut)]) k)
              (for/sum ([v (-in-immut-hash-values ht/immut)]) v))
           (+ (for/sum ([k (-in-mut-hash-keys ht/mut)]) k)
              (for/sum ([v (-in-mut-hash-values ht/mut)]) v))
           (+ (for/sum ([k (-in-weak-hash-keys ht/weak)]) k)
              (for/sum ([v (-in-weak-hash-values ht/weak)]) v))
           (+ (for/sum ([k (-in-immut-hash-keys fake-ht/immut)]) k)
              (for/sum ([v (-in-immut-hash-values fake-ht/immut)]) v))
           (+ (for/sum ([k (-in-mut-hash-keys fake-ht/mut)]) k)
              (for/sum ([v (-in-mut-hash-values fake-ht/mut)]) v))
           (+ (for/sum ([k (-in-weak-hash-keys fake-ht/weak)]) k)
              (for/sum ([v (-in-weak-hash-values fake-ht/weak)]) v))
           (+ (for/sum ([k ht/immut-keys/seq]) k)
              (for/sum ([v ht/immut-vals/seq]) v))
           (+ (for/sum ([k ht/mut-keys/seq]) k)
              (for/sum ([v ht/mut-vals/seq]) v))
           (+ (for/sum ([k ht/weak-keys/seq]) k)
              (for/sum ([v ht/weak-vals/seq]) v)))
          
          (test #t =
           (for/sum ([(k v) (-in-immut-hash ht/immut)]) k)
           (for/sum ([(k v) (-in-mut-hash ht/mut)]) k)
           (for/sum ([(k v) (-in-weak-hash ht/weak)]) k)
           (for/sum ([(k v) (-in-immut-hash fake-ht/immut)]) k)
           (for/sum ([(k v) (-in-mut-hash fake-ht/mut)]) k)
           (for/sum ([(k v) (-in-weak-hash fake-ht/weak)]) k)
           (for/sum ([(k v) ht/immut/seq]) k)
           (for/sum ([(k v) ht/mut/seq]) k)
           (for/sum ([(k v) ht/weak/seq]) k)
           (for/sum ([k+v (-in-immut-hash-pairs ht/immut)]) (car k+v))
           (for/sum ([k+v (-in-mut-hash-pairs ht/mut)]) (car k+v))
           (for/sum ([k+v (-in-weak-hash-pairs ht/weak)]) (car k+v))
           (for/sum ([k+v (-in-immut-hash-pairs fake-ht/immut)]) (car k+v))
           (for/sum ([k+v (-in-mut-hash-pairs fake-ht/mut)]) (car k+v))
           (for/sum ([k+v (-in-weak-hash-pairs fake-ht/weak)]) (car k+v))
           (for/sum ([k+v ht/immut-pair/seq]) (car k+v))
           (for/sum ([k+v ht/mut-pair/seq]) (car k+v))
           (for/sum ([k+v ht/weak-pair/seq]) (car k+v))
           (for/sum ([k (-in-immut-hash-keys ht/immut)]) k)
           (for/sum ([k (-in-mut-hash-keys ht/mut)]) k)
           (for/sum ([k (-in-weak-hash-keys ht/weak)]) k)
           (for/sum ([k (-in-immut-hash-keys fake-ht/immut)]) k)
           (for/sum ([k (-in-mut-hash-keys fake-ht/mut)]) k)
           (for/sum ([k (-in-weak-hash-keys fake-ht/weak)]) k)
           (for/sum ([k ht/immut-keys/seq]) k)
           (for/sum ([k ht/mut-keys/seq]) k)
           (for/sum ([k ht/weak-keys/seq]) k))
    
          (test #t =
           (for/sum ([(k v) (-in-immut-hash ht/immut)]) v)
           (for/sum ([(k v) (-in-mut-hash ht/mut)]) v)
           (for/sum ([(k v) (-in-weak-hash ht/weak)]) v)
           (for/sum ([(k v) (-in-immut-hash fake-ht/immut)]) v)
           (for/sum ([(k v) (-in-mut-hash fake-ht/mut)]) v)
           (for/sum ([(k v) (-in-weak-hash fake-ht/weak)]) v)
           (for/sum ([(k v) ht/immut/seq]) v)
           (for/sum ([(k v) ht/mut/seq]) v)
           (for/sum ([(k v) ht/weak/seq]) v)
           (for/sum ([k+v (-in-immut-hash-pairs ht/immut)]) (cdr k+v))
           (for/sum ([k+v (-in-mut-hash-pairs ht/mut)]) (cdr k+v))
           (for/sum ([k+v (-in-weak-hash-pairs ht/weak)]) (cdr k+v))
           (for/sum ([k+v (-in-immut-hash-pairs fake-ht/immut)]) (cdr k+v))
           (for/sum ([k+v (-in-mut-hash-pairs fake-ht/mut)]) (cdr k+v))
           (for/sum ([k+v (-in-weak-hash-pairs fake-ht/weak)]) (cdr k+v))
           (for/sum ([k+v ht/immut-pair/seq]) (cdr k+v))
           (for/sum ([k+v ht/mut-pair/seq]) (cdr k+v))
           (for/sum ([k+v ht/weak-pair/seq]) (cdr k+v))
           (for/sum ([v (-in-immut-hash-values ht/immut)]) v)
           (for/sum ([v (-in-mut-hash-values ht/mut)]) v)
           (for/sum ([v (-in-weak-hash-values ht/weak)]) v)
           (for/sum ([v (-in-immut-hash-values fake-ht/immut)]) v)
           (for/sum ([v (-in-mut-hash-values fake-ht/mut)]) v)
           (for/sum ([v (-in-weak-hash-values fake-ht/weak)]) v)
           (for/sum ([v ht/immut-vals/seq]) v)
           (for/sum ([v ht/mut-vals/seq]) v)
           (for/sum ([v ht/weak-vals/seq]) v))))]))
  (define-hash-iterations-tester generic
    in-hash in-hash-pairs in-hash-keys in-hash-values)
  (define-hash-iterations-tester specific
    in-immutable-hash in-mutable-hash in-weak-hash
    in-immutable-hash-pairs in-mutable-hash-pairs in-weak-hash-pairs
    in-immutable-hash-keys in-mutable-hash-keys in-weak-hash-keys
    in-immutable-hash-values in-mutable-hash-values in-weak-hash-values)
  
  (define lst1 (build-list 10 values))
  (define lst2 (build-list 10 add1))
  (test-hash-iters-generic lst1 lst2)
  (test-hash-iters-specific lst1 lst2)
  (define lst3 (build-list 100000 values))
  (define lst4 (build-list 100000 add1))
  (test-hash-iters-generic lst3 lst4)
  (test-hash-iters-specific lst3 lst4))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use keys that are a multile of a power of 2 to
; get "almost" collisions that force the hash table
; to use a deeper tree.

(let ()
  (define vals (for/list ([j (in-range 100)]) (add1 j)))
  (define sum-vals (for/sum ([v (in-list vals)]) v))
  (for ([shift (in-range 150)])
    (define keys (for/list ([j (in-range 100)])
                   (arithmetic-shift j shift)))
    ; test first the weak table to ensure the keys are not collected
    (define ht/weak (make-weak-hash (map cons keys vals)))
    (define sum-ht/weak (for/sum ([v (in-weak-hash-values ht/weak)]) v))
    (define ht/mut (make-hash (map cons keys vals)))
    (define sum-ht/mut (for/sum ([v (in-mutable-hash-values ht/mut)]) v))
    (define ht/immut (make-immutable-hash (map cons keys vals)))
    (define sum-ht/immut (for/sum ([v (in-immutable-hash-values ht/immut)]) v))
    (test #t = sum-vals sum-ht/weak sum-ht/mut sum-ht/immut)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define err-msg "no element at index")

;; Check that unsafe-weak-hash-iterate- ops do not segfault
;; when a key is collected before access; throw exception instead.
;; They are used for safe iteration in in-weak-hash- sequence forms
  (let ()
    (define ht #f)
    
    (let ([lst (build-list 10 add1)])
      (set! ht (make-weak-hash `((,lst . val)))))
    
    (define i (hash-iterate-first ht))
    
    ;; everything ok
    (test #t number? i)
    (test #t list? (hash-iterate-key ht i))
    (test #t equal? (hash-iterate-value ht i) 'val)
    (test #t equal? (cdr (hash-iterate-pair ht i)) 'val)
    (test #t equal? 
          (call-with-values (lambda () (hash-iterate-key+value ht i)) cons)
          '((1 2 3 4 5 6 7 8 9 10) . val))
    (test #f hash-iterate-next ht i)

    ;; collect key, everything should error
    (collect-garbage)
    (test #t boolean? (hash-iterate-first ht))
    (err/rt-test (hash-iterate-key ht i) exn:fail:contract? err-msg)
    (err/rt-test (hash-iterate-value ht i) exn:fail:contract? err-msg)
    (err/rt-test (hash-iterate-pair ht i) exn:fail:contract? err-msg)
    (err/rt-test (hash-iterate-key+value ht i) exn:fail:contract? err-msg)
    (test #f hash-iterate-next ht i))

;; Check that unsafe mutable hash table operations do not segfault
;; after getting valid index from unsafe-mutable-hash-iterate-first and -next.
;; Throw exception instead since they're used for safe iteration
  (let ()
    (define ht (make-hash '((a . b))))
    
    (define i (hash-iterate-first ht))
    
    ;; everything ok
    (test #t number? i)
    (test #t equal? (hash-iterate-key ht i) 'a)
    (test #t equal? (hash-iterate-value ht i) 'b)
    (test #t equal? (hash-iterate-pair ht i) '(a . b))
    (test #t equal? 
          (call-with-values (lambda () (hash-iterate-key+value ht i)) cons)
          '(a . b))
    (test #t boolean? (hash-iterate-next ht i))
    
    ;; remove element, everything should error
    (hash-remove! ht 'a)
    (test #t boolean? (hash-iterate-first ht))
    (err/rt-test (hash-iterate-key ht i) exn:fail:contract? err-msg)
    (err/rt-test (hash-iterate-value ht i) exn:fail:contract? err-msg)
    (err/rt-test (hash-iterate-pair ht i) exn:fail:contract? err-msg)
    (err/rt-test (hash-iterate-key+value ht i) exn:fail:contract? err-msg)
    (test #f hash-iterate-next ht i))
    

  (let ()
    (define ht (make-weak-hash '((a . b))))
    
    (define i (hash-iterate-first ht))
    
    ;; everything ok
    (test #t number? i)
    (test #t equal? (hash-iterate-key ht i) 'a)
    (test #t equal? (hash-iterate-value ht i) 'b)
    (test #t equal? (hash-iterate-pair ht i) '(a . b))
    (test #t equal? (call-with-values 
                        (lambda () (hash-iterate-key+value ht i)) cons)
                    '(a . b))
    (test #t boolean? (hash-iterate-next ht i))

    ;; remove element, everything should error
    (hash-remove! ht 'a)
    (test #t boolean? (hash-iterate-first ht))
    (err/rt-test (hash-iterate-key ht i) exn:fail:contract?)
    (err/rt-test (hash-iterate-value ht i) exn:fail:contract?)
    (err/rt-test (hash-iterate-pair ht i) exn:fail:contract?)
    (err/rt-test (hash-iterate-key+value ht i) exn:fail:contract?)
    (test #f hash-iterate-next ht i)))

;; ----------------------------------------

(define-syntax-rule (hash-remove-iterate-test make-hash (X ...) in-hash-X sel)
  (let ([ht (make-hash)])
    (arity-test in-hash-X 1 2)
    (test 'in-hash-X object-name in-hash-X)
    (define keys (for/list ([k (in-range 10)])
                   (gensym)))
    (define (add-keys!)
      (for ([k (in-list keys)]
            [i (in-naturals)])
        (hash-set! ht k i)))
    (add-keys!)
    (test 5 '(remove-during-loop make-hash in-hash-X)
          (for/sum ([(X ...) (in-hash-X ht #f)]
                    [i (in-naturals)])
            (when (= i 4)
              (for ([k (in-list keys)])
                (hash-remove! ht k)))
            (if (sel X ...) 1 0)))
    (add-keys!)
    (test 'ok '(remove-during-loop make-hash in-hash-X)
          (for/fold ([v 'ok]) ([(X ...) (in-hash-X ht #f)]
                               [i (in-naturals)])
            (when (= i 4)
              (set! keys #f)
              (collect-garbage))
            v))))

(define-syntax-rule (hash-remove-iterate-test* [make-hash ...] (X ...) in-hash-X in-Y-hash-X sel)
  (begin
    (hash-remove-iterate-test make-hash (X ...) in-hash-X sel) ...
    (hash-remove-iterate-test make-hash (X ...) in-Y-hash-X sel) ...))

(hash-remove-iterate-test* [make-hash make-hasheq make-hasheqv]
                          (k v) in-hash in-mutable-hash and)
(hash-remove-iterate-test* [make-hash make-hasheq make-hasheqv]
                          (k) in-hash-keys in-mutable-hash-keys values)
(hash-remove-iterate-test* [make-hash make-hasheq make-hasheqv]
                          (v) in-hash-values in-mutable-hash-values values)
(hash-remove-iterate-test* [make-hash make-hasheq make-hasheqv]
                           (p) in-hash-pairs in-mutable-hash-pairs car)

(hash-remove-iterate-test* [make-weak-hash make-weak-hasheq make-weak-hasheqv]
                          (k v) in-hash in-weak-hash and)
(hash-remove-iterate-test* [make-weak-hash make-weak-hasheq make-weak-hasheqv]
                          (k) in-hash-keys in-weak-hash-keys values)
(hash-remove-iterate-test* [make-weak-hash make-weak-hasheq make-weak-hasheqv]
                          (v) in-hash-values in-weak-hash-values values)
(hash-remove-iterate-test* [make-weak-hash make-weak-hasheq make-weak-hasheqv]
                           (p) in-hash-pairs in-weak-hash-pairs car)

;; ----------------------------------------

(report-errs)
