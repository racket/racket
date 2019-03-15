#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/contract
                 'racket/contract/collapsible)])

  (contract-eval
   '(define (add-many-contracts n ctc val [pos 'pos] [neg 'neg])
      (for/fold ([val val])
                ([i (in-range n)])
        (contract ctc val pos neg))))
  
  (contract-eval '(define ctc (vectorof integer?)))
  (contract-eval '(define (wrap x) (contract ctc x 'pos 'neg)))
  (contract-eval '(define vecof-one (wrap (wrap (vector 1)))))
  (contract-eval '(define bad-vecof-int (wrap (wrap (vector 'bad)))))

  (test/spec-passed
   'vec-collapsible1
   '(vector-ref vecof-one 0))
  (test/spec-passed
   'vec-collapsible2
   '(vector-set! vecof-one 0 2))
  (test/spec-failed
   'vec-collapsible3
   '(vector-set! vecof-one 0 'nan)
   'neg)
  (test/spec-failed
   'vec-collapsible4
   '(vector-ref bad-vecof-int 0)
   'pos)

  (test/spec-failed
   'vecof-bail-not-a-vector
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (contract ctc (add-many-contracts 11 ctc (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  (test/spec-failed
   'vec/c-bail-not-a-vector
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (contract ctc (add-many-contracts 11 ctc (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  (test/spec-failed
   'vec/c-different-lengths1
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c integer? integer?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  (test/spec-failed
   'vec/c-different-lengths2
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c integer? integer?)]
           [v (contract ctc2 (add-many-contracts 11 ctc1 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   'pos)

  (test/spec-failed
   'vec/c-different-lengths3
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c integer? integer?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 7))
   "inner-pos")

  (test/spec-failed
   'vec/c-different-lengths4
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c integer? integer?)]
           [v (contract ctc2 (add-many-contracts 11 ctc1 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 7))
   'pos)

  ;; Testing basic keyword arguments
  ;; ***********************************************

  ;; If the flat? argument is #t, then the resulting contract is a flat contract, and the c argument must also be a flat contract.
  ;; Such flat contracts will be unsound if applied to mutable vectors, as they will not check future operations on the vector.
  (test/spec-passed 
   'vec-collapsible-flat-passed
   '(let* ([ctc 	(vectorof integer? #:flat? #t)]
           [v 	(add-many-contracts 11
                                    ctc 
                                    (make-vector 10 42) 'pos 'neg )])
      (vector-ref v 1)))
  
  (test/spec-failed
   'vec-collapsible-flat-failed!
   '(let* ([ctc    (vectorof integer? #:flat? #t)]
           [v      (add-many-contracts 11
                                       ctc 
                                       (make-vector 10 "42") 'pos 'neg )])
      'oeps)
   "pos")
  
  ;;Should pass the test because we indicate that it is a flat contract
  (test/spec-passed
   'vec-collapsible-flat-set!
   '(let* ([ctc 	(vectorof integer? #:flat? #t)]
           [v 	(add-many-contracts
                 11
                 ctc 
                 (make-vector 10 42) 'pos 'neg )])
      (vector-set! v 1 "24")))
  
  ;If the immutable argument is #t and the c argument is a flat contract and the eager argument is #t, the result will be a flat contract. 
  (contract-eval  '(define ctc-flat (vectorof integer? #:immutable #t #:eager #t)))
  (test-true 'is-flat '(flat-contract? ctc-flat))
  
  (test/spec-failed
   'vec-collapsible-flat-set!
   '(let* ([ctc 	(vectorof integer? #:immutable #t #:eager #t)]
           [v 	(add-many-contracts
                 11
                 ctc 
                 (make-vector 10 42) 'pos 'neg )])
      'should-fail)
   "pos")
  
  (test/spec-passed
   'vec-collapsible-flat-set!
   '(let* ([ctc 	(vectorof integer? #:immutable #t #:eager #t)]
           [v 	(add-many-contracts
                 11
                 ctc 
                 (vector-immutable 10 42) 'pos 'neg )])
      (vector-ref v 1)))
  
   ;If the c argument is a chaperone contract, then the result will be a chaperone contract.
   (contract-eval '(define ctc-chap (vectorof (-> integer? integer?) #:immutable #t #:eager #t )))
   (test-true 'is-chaperone '(chaperone-contract? ctc-chap))


  (test/spec-passed
   'vec-collapsible-vector-chap
   '(let* ([ctc-chap    (vectorof (-> integer? integer?) #:immutable #t #:eager #t)]
           [v      (add-many-contracts
                    11
                    ctc-chap 
                    (vector-immutable (lambda (x) x)  (lambda (x) (* x x))) 'pos 'neg )])
      ((vector-ref v 1) 10)))
  
   (test/spec-failed
    'vec-collapsible-vector-chap-fail
    '(let* ([ctc-chap    (vectorof (-> integer? integer?) #:immutable #t #:eager #t)]
            [v      (add-many-contracts
                     11
                     ctc-chap 
                     (vector-immutable (lambda (x) "42")) 'pos 'neg )])
       ((vector-ref v 0) 10))
    "pos")

  ;; End basic keyword arguments
  ;; ***********************************************


  ;; non-flat contracts at the leaves/nested vectorof contracts
  (test/spec-passed
   'vec-collapsible5
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 0) (vector 1) (vector 2)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 1) 0)))

  (test/spec-failed
   'vec-collapsible6a
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (add-many-contracts 11 ctc (vector (vector 'bad)) 'inner-pos 'inner-neg)])
      (vector-ref (vector-ref v 0) 0))
   "inner-pos")
  
  (test/spec-failed
   'vec-collapsible6b
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 'bad)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0))
   "inner-pos")

  (test/spec-failed
   'vec-collapsible7
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! (vector-ref v 0) 0 'bad))
   'neg)

  (test/spec-failed
   'vec-collapsible8
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! v 0 (vector 'bad))
      (vector-ref (vector-ref v 0) 0))
   'neg)

  ;; non-identical contracts in nested vectorof
  (test/spec-failed
   'vec-collapsible9
   '(let* ([ctc1 (vectorof (vectorof integer?))]
           [ctc2 (vectorof (vectorof positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! (vector-ref v 0) 0 -1))
   "inner-neg")

  (test/spec-failed
   'vec-collapsible10
   '(let* ([ctc1 (vectorof (vectorof integer?))]
           [ctc2 (vectorof (vectorof positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! (vector-ref v 0) 0 1/2))
   'neg)

  (test/spec-failed
   'vec-collapsible11
   '(let* ([ctc1 (vectorof (vectorof integer?))]
           [ctc2 (vectorof (vectorof positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector 1/2)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0))
   'pos)

  (test/spec-failed
   'vec-collapsible12
   '(let* ([ctc1 (vectorof (vectorof integer?))]
           [ctc2 (vectorof (vectorof positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector -1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0))
   "inner-pos")

  ;; tests for various first-order checks performed by vectors
  (test/spec-failed
   'vec-collapsible13
   '(let* ([ctc [vectorof (vectorof integer?)]]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! v 0 'bad))
   'neg)

    (test/spec-failed
   'vec-collapsible14
   '(let* ([ctc (vectorof (vectorof integer? #:immutable #t))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  (test/spec-passed
   'vectorof-impersonator
   '(let* ([ctc (vectorof (make-contract #:late-neg-projection (lambda (b) (lambda (x n) 'foo))))]
           [v (contract ctc (add-many-contracts 11 ctc (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; vector/c contract tests
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test/spec-failed
   'vector/c-bad-index
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1 2)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  (test/spec-passed
   'vector/c-collapsible1
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 0)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0)))

  (test/spec-failed
   'vector/c-collapsible2
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (add-many-contracts 11 ctc (vector (vector 'bad)) 'inner-pos 'inner-neg)])
      (vector-ref (vector-ref v 0) 0))
   "inner-pos")

  (test/spec-failed
   'vector/c-collapsible3
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 'bad)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0))
   "inner-pos")

  (test/spec-failed
   'vector/c-collapsible4
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! (vector-ref v 0) 0 'bad))
   'neg)

  (test/spec-failed
   'vector/c-collapsible5
   '(let* ([ctc (vector/c (vector/c integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! v 0 (vector 'bad))
      (vector-ref (vector-ref v 0) 0))
   'neg)

  ;; non-identical contracts in nested vectorof
  (test/spec-failed
   'vector/c-collapsible6
   '(let* ([ctc1 (vector/c (vector/c integer?))]
           [ctc2 (vector/c (vector/c positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! (vector-ref v 0) 0 -1))
   "inner-neg")

  (test/spec-failed
   'vector/c-collapsible7
   '(let* ([ctc1 (vector/c (vector/c integer?))]
           [ctc2 (vector/c (vector/c positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! (vector-ref v 0) 0 1/2))
   'neg)

  (test/spec-failed
      'vector/c-collapsible8
   '(let* ([ctc1 (vector/c (vector/c integer?))]
           [ctc2 (vector/c (vector/c positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector 1/2)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0))
   'pos)

  (test/spec-failed
   'vector/c-collapsible9
   '(let* ([ctc1 (vector/c (vector/c integer?))]
           [ctc2 (vector/c (vector/c positive?))]
           [v (contract
               ctc1
               (add-many-contracts 11 ctc2 (vector (vector -1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref (vector-ref v 0) 0))
   "inner-pos")

  ;; tests for various first-order checks performed by vectors
  (test/spec-failed
   'vector/c-collapsible10
   '(let* ([ctc [vector/c (vector/c integer?)]]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-set! v 0 'bad))
   'neg)

    (test/spec-failed
     'vector/c-collapsible11
     '(let* ([ctc (vector/c (vector/c integer? #:immutable #t))]
             [v (contract
                 ctc
                 (add-many-contracts 11 ctc (vector (vector 1)) 'inner-pos 'inner-neg)
                 'pos 'neg)])
        (vector-ref v 0))
     "inner-pos")

  (test/spec-passed
   'vector/c-impersonator
   '(let* ([ctc (vector/c (make-contract #:late-neg-projection (lambda (b) (lambda (x n) 'foo))))]
           [v (contract ctc (add-many-contracts 11 ctc (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0)))

  (test/spec-failed
   'vector/c-blame
   '(let* ([ctc (vector/c (-> integer? integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector add1) 'inner-pos 'inner-neg)
               'pos 'neg)])
      ((vector-ref v 0) 1.5))
   "neg")

  (test/spec-failed
   'vectorof-blame
   '(let* ([ctc (vectorof (-> integer? integer?))]
           [v (contract
               ctc
               (add-many-contracts 11 ctc (vector add1) 'inner-pos 'inner-neg)
               'pos 'neg)])
      ((vector-ref v 0) 1.5))
   "neg")

  ;; collapsible continuation marks

  (contract-eval
   '(define (make-has-collapsible-mark? b)
      (flat-named-contract
       'has-collapsible-mark?
       (lambda (v)
         (define marks (current-continuation-marks))
         (define res (continuation-mark-set-first marks collapsible-contract-continuation-mark-key))
         (set-box! b (or (unbox b) res))
         #t))))

  (test-true
   'collapsible-mark-present
   '(let* ([b (box #f)]
           [ctc (vectorof (make-has-collapsible-mark? b))]
           [v (add-many-contracts 12 ctc (vector 1))])
      (vector-ref v 0)
      (unbox b)))

  (test/spec-passed/result
   'collapsible-mark-absent
   '(let* ([b (box #f)]
           [ctc (vectorof (make-has-collapsible-mark? b))]
           [v (contract ctc (vector 1) 'pos 'neg)])
      (vector-ref v 0)
      (unbox b))
   #f
   do-not-double-wrap)

  (test/spec-failed
   'vector/c-bailout
   '(let* ([ctc1 (vector/c (vector/c integer?))]
           [ctc2 (vector/c (-> integer?))]
           [v (contract
               ctc2
               (add-many-contracts
                11
                ctc1
                (vector (vector 1 2))
                'inner-pos 'inner-neg)
               'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Implementation tests
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval '(require (submod racket/contract/private/vector-collapsible for-testing)))
  (contract-eval '(require (submod racket/contract/private/collapsible-common for-testing)))
  (contract-eval '(define (collapsible? val)
                    (and (has-impersonator-prop:collapsible? val)
                         (let ([prop (get-impersonator-prop:collapsible val #f)])
                           (and (collapsible-wrapper-property? prop)
                                (eq? val (collapsible-property-ref prop)))))))

  ;; vectorof
  (contract-eval
   '(define (vectorof-has-num-contracts? v ref set)
      (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
        (unless (has-impersonator-prop:collapsible? v)
          (error "vectorof-has-num-contracts?: no collapsible-contract"))
        ;; TODO: maybe should check that is a collapsible-wrapper-property ...
        (define collapsible/c (collapsible-property-c-c (get-impersonator-prop:collapsible v)))
        (define ref/c (collapsible-vector-ref-ctcs collapsible/c))
        (define set/c (collapsible-vector-set-ctcs collapsible/c))
        (unless (= (length (collapsible-leaf/c-proj-list ref/c)) ref)
          (error "vectorof-has-num-contracts?: wrong number of ref projections"))
        (unless (= (length (collapsible-leaf/c-proj-list set/c)) set)
          (error "vectorof-has-num-contracts?: wrong number of set projections"))
        (unless (= (length (collapsible-leaf/c-contract-list ref/c)) ref)
          (error "vectorof-has-num-contracts?: wrong number of ref contracts"))
        (unless (= (length (collapsible-leaf/c-contract-list set/c)) set)
          (error "vectorof-has-num-contracts?: wrong number of set contracts"))
        #t)))

  (contract-eval
   '(define (vector-can-combine? val ctc)
      (define cv (contract ctc val 'p 'n))
      (and (collapsible? val)
           (collapsible? cv))))

  ;; vector/c
  (contract-eval
   '(define (vector/c-has-num-contracts? v refs sets)
      (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
        (unless (has-impersonator-prop:collapsible? v)
          (error "vectorof-has-num-contracts?: no collapsible-contract"))
        ;; TODO: maybe should check that is a collapsible-wrapper-property ...
        (define collapsible/c (collapsible-property-c-c (get-impersonator-prop:collapsible v)))
        (define ref-ctcs (collapsible-vector-ref-ctcs collapsible/c))
        (define set-ctcs (collapsible-vector-set-ctcs collapsible/c))
        (for ([ref (in-list refs)]
              [ref/c (in-vector ref-ctcs)])
          (unless (= (length (collapsible-leaf/c-proj-list ref/c)) ref)
            (error "vector/c-has-num-contracts?: wrong number of ref projections"))
          (unless (= (length (collapsible-leaf/c-contract-list ref/c)) ref)
            (error "vector/c-has-num-contracts?: wrong number of ref contracts")))
        (for ([set (in-list sets)]
              [set/c (in-vector set-ctcs)])
          (unless (= (length (collapsible-leaf/c-proj-list set/c)) set)
            (error "vector/c-has-num-contracts?: wrong number of set projections"))
          (unless (= (length (collapsible-leaf/c-contract-list set/c)) set)
            (error "vector/c-has-num-contracts?: wrong number of set contracts")))
        #t)))
   
  (contract-eval
   '(define pos
      (flat-named-contract
       'pos
       (lambda (x) (and (integer? x) (>= x 0))))))
  
  (test-true
   'vecof-false-contracts
   '(let* ([cv (add-many-contracts 11 (vectorof #f) (vector #f))])
      (vectorof-has-num-contracts? cv 1 1)))

  (test-true
   'vec/c-false-contracts
   '(let* ([cv (add-many-contracts 11 (vector/c #f) (vector #f))])
      (vector/c-has-num-contracts? cv '(1) '(1))))

  (test-true
   'vecof-many-false-contracts
   '(vectorof-has-num-contracts? (add-many-contracts 1000 (vectorof #f) (vector #f)) 1 1))

  (test-true
   'vec/c-many-false-contracts
   '(vector/c-has-num-contracts? (add-many-contracts 1000 (vector/c #f) (vector #f)) '(1) '(1)))

  (test-true
   'vecof-num-contracts
   '(let* ([v (add-many-contracts 11 (vectorof pos) (vector 1))])
      (vectorof-has-num-contracts? v 1 1)))

  (test-true
   'vecof-num-contracts-different-ref-set
   '(let* ([ctc1 (vectorof (>/c 0))]
           [ctc2 (vectorof real?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1))]
           [v (contract ctc2 (contract ctc2 v1 'pos 'neg) 'pos 'neg)])
      (vectorof-has-num-contracts? v 1 2)))

  (test-true
   'vec/c-num-contracts
   '(let* ([v (add-many-contracts 11 (vector/c pos pos) (vector 1 2))])
      (vector/c-has-num-contracts? v '(1 1) '(1 1))))

  (test-true
   'vec/c-num-contracts-different-ref-set
   '(let* ([ctc1 (vector/c (>/c 0) (>/c 0))]
           [ctc2 (vector/c real? real?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1 2))]
           [v (contract ctc2 (contract ctc2 v1 'pos 'neg) 'pos 'neg)])
      (vector/c-has-num-contracts? v '(1 1) '(2 2))))

  (test-true
   'vec/c-num-contracts-different-ref-set-different-posns
   '(let* ([ctc1 (vector/c (>/c 0) real?)]
           [ctc2 (vector/c real? real?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1 2))]
           [v (contract ctc2 (contract ctc2 v1 'pos 'neg) 'pos 'neg)])
      (vector/c-has-num-contracts? v '(1 1) '(2 1))))

   (test-true
   'vec/c-more-ref-than-set
   '(let* ([ctc2 (vector/c (>/c 0) real?)]
           [ctc1 (vector/c real? real?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1 2))]
           [v (contract ctc2 (contract ctc2 v1 'pos 'neg) 'pos 'neg)])
      (vector/c-has-num-contracts? v '(2 1) '(1 1))))

  ;; TODO: a couple more has-num-contracts? tests with one contract sandwiching another for more interesting contract
  ;; merging

  (test-true
   'vecof-sandwich1
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof real?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1))]
           [v (contract ctc1 (contract ctc2 v1 'p 'n) 'p 'n)])
      (vectorof-has-num-contracts? v 2 2)))

  (test-true
   'vec/c-sandwich1
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c real?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1))]
           [v (contract ctc1 (contract ctc2 v1 'p 'n) 'p 'n)])
      (vector/c-has-num-contracts? v '(2) '(2))))


  (test-true
   'vecof-incompatible1
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof string?)]
           [v1 (add-many-contracts 11 ctc2 (vector 1))]
           [v (contract ctc1 v1 'p 'n)])
      (vectorof-has-num-contracts? v 2 2)))

  (test-true
   'vecof-incompatible2
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof string?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1))]
           [v (contract ctc2 v1 'p 'n)])
      (vectorof-has-num-contracts? v 2 2)))

  (test-true
   'vec/c-incompatible1
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c string?)]
           [v1 (add-many-contracts 11 ctc2 (vector 1))]
           [v (contract ctc1 v1 'p 'n)])
      (vector/c-has-num-contracts? v '(2) '(2))))

  (test-true
   'vec/c-incompatible2
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c string?)]
           [v1 (add-many-contracts 11 ctc1 (vector 1))]
           [v (contract ctc2 v1 'p 'n)])
      (vector/c-has-num-contracts? v '(2) '(2))))

  (test/spec-failed
   'vecof-incompatible1-blame1
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")
  (test/spec-failed
   'vecof-incompatible1-blame2
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector "foo") 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   'pos)
  (test/spec-failed
   'vecof-incompatible1-blame3
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 "foo"))
   'neg)
  (test/spec-failed
   'vecof-incompatible1-blame4
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 2))
   "inner-neg")

  (test/spec-failed
   'vec/c-incompatible1-blame1
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")
  (test/spec-failed
   'vec/c-incompatible1-blame2
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector "foo") 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   'pos)
  (test/spec-failed
   'vec/c-incompatible1-blame3
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 "foo"))
   'neg)
  (test/spec-failed
   'vec/c-incompatible1-blame4
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c string?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 2))
   "inner-neg")

  ;; can combine tests

  (contract-eval
   '(define imp-ctc1
      (make-contract
       #:late-neg-projection (lambda (blame) (lambda (val neg) val)))))

  (contract-eval
   '(define imp-ctc2
      (make-contract
       #:late-neg-projection (lambda (blame) (lambda (val neg) val)))))

  (contract-eval
   '(define chap-ctc
      (make-chaperone-contract
       #:late-neg-projection (lambda (blame) (lambda (val neg) val)))))

  ;; vectorof combine
  (test/spec-passed/result
   'vectorof-can-combine-chaps
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vectorof real?)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #t)

  (test/spec-passed/result
   'vectorof-can-combine-imps
   '(let* ([ctc1 (vectorof imp-ctc1)]
           [ctc2 (vectorof imp-ctc2)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #t)

  (test/spec-passed/result
   'vectorof-cant-mix-chap-imp
   '(let* ([ctc1 (vectorof chap-ctc)]
           [ctc2 (vectorof imp-ctc1)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #f)

  (test/spec-passed/result
   'vectorof-cant-mix-imp-chap
   '(let* ([ctc1 (vectorof imp-ctc1)]
           [ctc2 (vectorof chap-ctc)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #f)

  (test/spec-passed/result
   'vectorof-cant-merge-if-chaperoned-in-se-mode
   '(let* ([ctc (vectorof integer?)]
           [v1 (add-many-contracts 11 ctc (vector 1) 'pos 'neg)]
           [v (chaperone-vector v1 #f #f)])
      (vector-can-combine? v ctc))
   #f)

  ;; vector/c combine
  (test/spec-passed/result
   'vector/c-can-combine-chaps
   '(let* ([ctc1 (vector/c integer?)]
           [ctc2 (vector/c real?)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #t)

  (test/spec-passed/result
   'vector/c-can-combine-imps
   '(let* ([ctc1 (vector/c imp-ctc1)]
           [ctc2 (vector/c imp-ctc2)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #t)

  (test/spec-passed/result
   'vector/c-cant-mix-chap-imp
   '(let* ([ctc1 (vector/c chap-ctc)]
           [ctc2 (vector/c imp-ctc1)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #f)

  (test/spec-passed/result
   'vector/c-cant-mix-imp-chap
   '(let* ([ctc1 (vector/c imp-ctc1)]
           [ctc2 (vector/c chap-ctc)]
           [v (add-many-contracts 11 ctc1 (vector 1) 'pos 'neg)])
      (vector-can-combine? v ctc2))
   #f)

  (test/spec-passed/result
   'vector/c-cant-merge-if-chaperoned-in-se-mode
   '(let* ([ctc (vector/c integer?)]
           [v1 (add-many-contracts 11 ctc (contract ctc (vector 1) 'pos 'neg) 'pos 'neg)]
           [v (chaperone-vector v1 #f #f)])
      (vector-can-combine? v ctc))
   #f)


  (contract-eval
   '(define many-layers
      (contract (vectorof even?)
                (chaperone-vector
                 (add-many-contracts
                  11
                  (vectorof exact-integer?)
                  (contract (vectorof positive?)
                            (vector 2)
                            'pos1 'neg1)
                  'pos2 'neg2)
                 #f
                 #f)
                'pos3 'neg3)))

  (test/spec-failed
   'many-layers-neg1
   '(vector-set! many-layers 0 0)
   "neg1")
  (test/spec-failed
   'many-layers-neg2
   '(vector-set! many-layers 0 2.0)
   "neg2")
  (test/spec-failed
   'many-layers-neg3
   '(vector-set! many-layers 0 1)
   "neg3")

  (contract-eval
   '(define many-layers/c
      (contract (vector/c even?)
                (chaperone-vector
                 (add-many-contracts
                  11
                  (vector/c exact-integer?)
                  (contract (vector/c positive?)
                            (vector 2)
                            'pos1 'neg1)
                  'pos2 'neg2)
                 #f
                 #f)
                'pos3 'neg3)))
  
  (test/spec-failed
   'many-layers/c-neg1
   '(vector-set! many-layers/c 0 0)
   "neg1")
  (test/spec-failed
   'many-layers/c-neg2
   '(vector-set! many-layers/c 0 2.0)
   "neg2")
  (test/spec-failed
   'many-layers/c-neg3
   '(vector-set! many-layers/c 0 1)
   "neg3")

  ;; Vector Sorting Tests
  ;; Make sure that if we sort a vector of vectors
  ;; that must ref each element at least n times that the
  ;; contained vectors do not build up contracts
  
  (contract-eval
   '(define (my-sort vec)
      (define length (vector-length vec))
      (for ([i (in-range length)])
        (for ([j (in-range i length)])
          (define vi (vector-ref vec i))
          (define vj (vector-ref vec j))
          (when (< (vector-ref vj 0) (vector-ref vi 0))
            (vector-set! vec i vj)
            (vector-set! vec j vi))))))

  (contract-eval
   '(define unsorted (vector
                      (vector 10)
                      (vector 9)
                      (vector 8)
                      (vector 7)
                      (vector 6)
                      (vector 5)
                      (vector 4)
                      (vector 3)
                      (vector 2)
                      (vector 1))))

  (contract-eval
   '(define unsorted+contracted
      (add-many-contracts 11 (vectorof (vectorof integer?))
                          unsorted
                          'pos 'neg)))

  (test-true
   'vecof-sorting
   '(let ()
      (my-sort unsorted+contracted)
      (for/and ([v (in-vector unsorted+contracted)])
        (vectorof-has-num-contracts? v 1 1))))

  (contract-eval
   '(define unsorted2 (vector
                      (vector 10)
                      (vector 9)
                      (vector 8)
                      (vector 7)
                      (vector 6)
                      (vector 5)
                      (vector 4)
                      (vector 3)
                      (vector 2)
                      (vector 1))))

  (contract-eval
   '(define unsorted+contracted-vector/c
      (add-many-contracts 11
                          (vector/c (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?)
                                    (vector/c integer?))
                          unsorted2
                          'pos 'neg)))

  (test-true
   'vecof-sorting
   '(let ()
      (my-sort unsorted+contracted-vector/c)
      (for/and ([v (in-vector unsorted+contracted-vector/c)])
        (vector/c-has-num-contracts? v '(1) '(1)))))

  ;; bail out and switching bugs

  (contract-eval '(require racket/class))

  (test/spec-passed
   'object/c-passing-vecof
   '(let* ([grid/c (vectorof (vectorof (object/c)))]
           [grid (contract
                  grid/c
                  (add-many-contracts
                   11
                   grid/c
                   (vector (vector (new object%)))
                   'inner-pos 'inner-neg)
                  'pos 'neg)])
      (vector-ref (vector-ref grid 0) 0)))

  (test/spec-failed
   'object/c-failing-vecof
   '(let* ([v (contract (vectorof integer?) (vector 1) 'orig-p 'orig-n)]
           [grid/c (vectorof (vectorof (object/c)))]
           [grid (contract
                  grid/c
                  (add-many-contracts
                   11
                   grid/c
                   (vector v)
                   'inner-pos 'inner-neg)
                  'pos 'neg)])
      (vector-ref (vector-ref grid 0) 0))
   "inner-pos")

  (test/spec-passed
   'object/c-passing-vec/c
   '(let* ([grid/c (vector/c (vector/c (object/c)))]
           [grid (contract
                  grid/c
                  (add-many-contracts
                   11
                   grid/c
                   (vector (vector (new object%)))
                   'inner-pos 'inner-neg)
                  'pos 'neg)])
      (vector-ref (vector-ref grid 0) 0)))

  (test/spec-failed
   'object/c-failing-vecof
   '(let* ([v (contract (vector/c integer?) (vector 1) 'orig-p 'orig-n)]
           [grid/c (vector/c (vector/c (object/c)))]
           [grid (contract
                  grid/c
                  (add-many-contracts
                   11
                   grid/c
                   (vector v)
                   'inner-pos 'inner-neg)
                  'pos 'neg)])
      (vector-ref (vector-ref grid 0) 0))
   "inner-pos")

  (contract-eval
   '(define (double-wrapped? x)
      (define prop (get-impersonator-prop:collapsible x #f))
      (and
       (collapsible-wrapper-property? prop)
       (and (has-impersonator-prop:collapsible?
             (collapsible-wrapper-property-checking-wrapper prop))
            ;; this is annoying because of how unsafe-chaperone-vector ...
            ;; work in relation to impersonator-properties
            (collapsible-wrapper-property?
             (get-impersonator-prop:collapsible
              (collapsible-wrapper-property-checking-wrapper prop)
              #f))))))

  (test-false
   'dont-multi-wrap
   '(let* ([ctc (vectorof (vectorof integer?))]
           [v (contract ctc (add-many-contracts 11 ctc (vector (vector 1)) 'ip 'in) 'p 'n)]
           [v2 (contract (vectorof any/c) v 'p2 'n2)])
      (double-wrapped? v2)))

  ;; blame and higher-order leaves
  (test/spec-failed
   'vectorof+box/c-different-blame
   '(let* ([ctc1 (vectorof (box/c integer?))]
           [v (contract ctc1
                        (add-many-contracts
                         11
                         ctc1
                         (vector (box 1))
                         'inner-pos 'inner-neg)
                        'pos 'neg)])
      (set-box! (vector-ref v 0) 1.1))
   "neg")

  (test/spec-failed
   'vectorof+box/c-same-blame
   '(let* ([ctc1 (vectorof (box/c real? any/c))]
           [ctc2 (vectorof (box/c (>/c 0) any/c))]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector (box 1))
                                       'pos 'neg) 'pos 'neg)])
      (set-box! (vector-ref v 0) -1))
   "neg")

  (test/spec-failed
   'vectorof+box/c-same-blame2
   '(let* ([ctc1 (vectorof (box/c real? any/c))]
           [ctc2 (vectorof (box/c (>/c 0) any/c))]
           [v (contract ctc2 (add-many-contracts 11 ctc1 (vector (box 1))
                                       'pos 'neg) 'pos 'neg)])
      (set-box! (vector-ref v 0) -1))
   "neg")

  (test/spec-failed
   'vectorof+box
   '(let* ([ctc (vectorof (box/c integer?))]
           [v (contract ctc (add-many-contracts 11 ctc (vector (box 1)) 'inner-pos 'inner-neg) 'pos 'neg)])
      (set-box! (vector-ref v 0) 1.5))
   "neg")

  (test/spec-failed
   'vector/c+box
   '(let* ([ctc (vector/c (box/c integer?))]
           [v (contract ctc (add-many-contracts 11 ctc (vector (box 1)) 'inner-pos 'inner-neg) 'pos 'neg)])
      (set-box! (vector-ref v 0) 1.5))
   "neg")

  ;; Tests for nested merging of vectorof and vector/c contracts
  (test/spec-passed/result
   'vecof+vec/c1
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vector/c integer? integer?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1 2) 'ip 'in) 'p 'n)])
      (and (vector/c-has-num-contracts? v '(1 1) '(1 1))
           (collapsible? v)))
   #t)

  (test/spec-passed/result
   'vecof+vec/c2
   '(let* ([ctc2 (vectorof integer?)]
           [ctc1 (vector/c integer? integer?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1 2) 'ip 'in) 'p 'n)])
      (and (vector/c-has-num-contracts? v '(1 1) '(1 1))
           (collapsible? v)))
   #t)

  (test/spec-failed
   'vecof+vec/c3
   '(let* ([ctc1 (vectorof integer?)]
           [ctc2 (vector/c integer? integer?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1 2 3) 'ip 'in) 'p 'n)])
      v)
   "ip")

  (test/spec-failed
   'vecof+vec/c4
   '(let* ([ctc2 (vectorof integer?)]
           [ctc1 (vector/c integer? integer?)]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector 1 2 3) 'ip 'in) 'p 'n)])
      v)
   "p")

  (test/spec-passed/result
   'vecof+vec/c5
   '(let* ([ctc2 (vectorof (vector/c integer?))]
           [ctc1 (vector/c (vector/c integer?))]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector (vector 1)) 'ip 'in) 'p 'n)]
           [v1 (vector-ref v 0)])
      (and (vector/c-has-num-contracts? v1 '(1) '(1))
           (collapsible? v)
           (collapsible? v1)))
   #t)

  (test/spec-passed/result
   'vecof+vec/c6
   '(let* ([ctc1 (vectorof (vector/c integer?))]
           [ctc2 (vector/c (vector/c integer?))]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector (vector 1)) 'ip 'in) 'p 'n)]
           [v1 (vector-ref v 0)])
      (and (vector/c-has-num-contracts? v1 '(1) '(1))
           (collapsible? v)
           (collapsible? v1)))
   #t)

  (test/spec-passed/result
   'vecof+vec/c7
   '(let* ([ctc1 (vectorof (vector/c integer?))]
           [ctc2 (vector/c (vectorof integer?))]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector (vector 1)) 'ip 'in) 'p 'n)]
           [v1 (vector-ref v 0)])
      (and (vector/c-has-num-contracts? v1 '(1) '(1))
           (collapsible? v)
           (collapsible? v1)))
   #t)

  (test/spec-passed/result
   'vecof+vec/c8
   '(let* ([ctc2 (vectorof (vector/c integer?))]
           [ctc1 (vector/c (vectorof integer?))]
           [v (contract ctc1 (add-many-contracts 11 ctc2 (vector (vector 1)) 'ip 'in) 'p 'n)]
           [v1 (vector-ref v 0)])
      (and (vector/c-has-num-contracts? v1 '(1) '(1))
           (collapsible? v)
           (collapsible? v1)))
   #t)

  (test/spec-failed
   'vecof+vec/c9
   '(let* ([ctc1 (vectorof (vector/c (vectorof integer?)))]
           [ctc2 (vector/c (vectorof (vector/c integer?)))]
           [v
            (contract
             ctc1
             (add-many-contracts 11 ctc2 (vector (vector (vector 1 2))) 'ip 'in)
             'p 'n)]
           [v0 (vector-ref v 0)])
      (vector-ref v0 0))
   "ip")

  (test/spec-failed
   'vecof+vec/c10
   '(let* ([ctc1 (vectorof (vector/c (vectorof integer?)))]
           [ctc2 (vector/c (vectorof (vector/c integer?)))]
           [v
            (contract
             ctc2
             (add-many-contracts 11 ctc1 (vector (vector (vector 1 2))) 'ip 'in)
             'p 'n)]
           [v0 (vector-ref v 0)])
      (vector-ref v0 0))
   "p")

  (test/spec-failed
   'vecof+vec/c11
   '(let* ([ctc1 (vectorof (vector/c (vectorof integer?)))]
           [ctc2 (vector/c (vectorof (vector/c integer?)))]
           [v
            (contract
             ctc1
             (add-many-contracts 11 ctc2 (vector (vector (vector 1))) 'ip 'in)
             'p 'n)]
           [v0 (vector-ref v 0)])
      (vector-set! v0 0 (vector 2 3)))
   "in")

   (test/spec-failed
   'vecof+vec/c12
   '(let* ([ctc1 (vectorof (vector/c (vectorof integer?)))]
           [ctc2 (vector/c (vectorof (vector/c integer?)))]
           [v
            (contract
             ctc2
             (add-many-contracts 11 ctc1 (vector (vector (vector 1))) 'ip 'in)
             'p 'n)]
           [v0 (vector-ref v 0)])
      (vector-set! v0 0 (vector 2 3)))
   "n")

  (test/spec-passed/result
   'vecof+vec/c13
   '(let* ([ctc1 (vectorof (vector/c (vectorof integer?)))]
           [ctc2 (vector/c (vectorof (vector/c integer?)))]
           [v
            (contract
             ctc1
             (add-many-contracts 11 ctc2 (vector (vector (vector 1))) 'ip 'in)
             'p 'n)]
           [v0 (vector-ref v 0)]
           [v1 (vector-ref v0 0)])
      (and (vector/c-has-num-contracts? v1 '(1) '(1))
           (collapsible? v)
           (collapsible? v0)
           (collapsible? v1)))
   #t)

  (test-true
   'vecof+vec/c14
   '(let* ([ctc1 (vectorof (vector/c (>/c 0) (>/c 0)))]
           [ctc2 (vector/c (vectorof real?))]
           [v1 (add-many-contracts 11 ctc1 (vector (vector 1 2)) 'ip 'in)]
           [v2 (add-many-contracts 11  ctc2 v1 'p 'n)]
           [v (vector-ref v2 0)])
      (vector/c-has-num-contracts? v '(1 1) '(2 2))))

  (test-true
   'vecof+vec/c15
   '(let* ([ctc1 (vectorof (vector/c real? real?))]
           [ctc2 (vector/c (vectorof (>/c 0)))]
           [v1 (add-many-contracts 11 ctc1 (vector (vector 1 2)) 'ip 'in)]
           [v2 (add-many-contracts 11 ctc2 v1 'p 'n)]
           [v (vector-ref v2 0)])
      (vector/c-has-num-contracts? v '(2 2) '(1 1))))

  (test/spec-passed
   'vecof+chap+non-c-c
   '(let ()
      (define ctc (vectorof (-> integer?)))
      (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda () 1)) 'ip 'in) 'p 'n))
      (define my/c
        (make-chaperone-contract
         #:late-neg-projection
         (lambda (blame)
           (lambda (val neg)
             (define full-blame (blame-add-missing-party blame neg))
             (chaperone-procedure
              val
              #f
              impersonator-prop:contracted my/c
              impersonator-prop:blame full-blame)))))
      (define f (contract my/c (lambda () 23) 'pf 'nf))
      (vector-set! v 0 f)))


  (contract-eval
   '(define my->/c
      (make-chaperone-contract
       #:late-neg-projection
       (lambda (blame)
         (lambda (val neg)
           (define full-blame (blame-add-missing-party blame neg))
           (chaperone-procedure
            val
            (lambda (arg)
              (unless (integer? arg)
                (raise-blame-error (blame-swap full-blame) arg "bad arg"))
              (values
               (lambda (res)
                 (unless (integer? res)
                   (raise-blame-error full-blame res "bad res"))
                 res)
               arg))
            impersonator-prop:contracted my->/c
            impersonator-prop:blame full-blame))))))

   (test/spec-passed
    'vecof+chap+non-c-c-ok
    '(let ()
       (define ctc (vectorof (-> integer? integer?)))
       (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda (x) 1)) 'ip 'in) 'p 'n))
       (define f (contract my->/c (lambda (x) 23) 'pf 'nf))
       (vector-set! v 0 f)
       ((vector-ref v 0) 1)))

   (test/spec-failed
    'vecof+chap+non-c-c-blame-neg
    '(let ()
       (define ctc (vectorof (-> integer? integer?)))
       (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda (x) 1)) 'ip 'in) 'p 'n))
       (define f (contract my->/c (lambda (x) 23) 'pf 'nf))
       (vector-set! v 0 f)
       ((vector-ref v 0) 1.1))
    "n")

   (test/spec-failed
    'vecof+chap+non-c-c-blame-pos
    '(let ()
       (define ctc (vectorof (-> integer? integer?)))
       (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda (x) 1)) 'ip 'in) 'p 'n))
       (define f (contract my->/c (lambda (x) 2.3) 'pf 'nf))
       (vector-set! v 0 f)
       ((vector-ref v 0) 1))
    "pf")

  (test/spec-passed
   'vecof+->-insert-contracted-non-c-c
   '(let ()
      (define ctc (vectorof (-> integer?)))
      (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda () 1)) 'ip 'in) 'p 'n))
      (define f (contract
                 (case-> (-> integer?)
                         (-> (values integer? integer?)))
                 (lambda () 2)
                 'fp 'fn))
      (vector-set! v 0 f)))

  (test/spec-passed
   'vec/c+->-insert-contracted-non-c-c
   '(let ()
      (define ctc (vector/c (-> integer?)))
      (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda () 1)) 'ip 'in) 'p 'n))
      (define f (contract
                 (case-> (-> integer?)
                         (-> (values integer? integer?)))
                 (lambda () 2)
                 'fp 'fn))
      (vector-set! v 0 f)))

  (test/spec-failed
   'vecof+->-insert-contracted-non-c-c-fail-blame-pos
   '(let ()
      (define ctc (vectorof (-> integer?)))
      (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda () 1)) 'ip 'in) 'p 'n))
      (define f (contract
                 (case-> (-> integer?)
                         (-> (values integer? integer?)))
                 (lambda () 'bad)
                 'fp 'fn))
      (vector-set! v 0 f)
      ((vector-ref v 0)))
   "fp")

  (test/spec-failed
   'vecof+->-insert-contracted-non-c-c-fail-blame-neg
   '(let ()
      (define ctc (vectorof (-> integer? integer?)))
      (define v (contract ctc (add-many-contracts 11 ctc (vector (lambda (x) 1)) 'ip 'in) 'p 'n))
      (define f (contract
                 (case-> (-> integer? integer?)
                         (-> integer? (values integer? integer?)))
                 (lambda (x) 3)
                 'fp 'fn))
      (vector-set! v 0 f)
      ((vector-ref v 0) 1.2))
   "n")

  (test/spec-failed
   'vector-symbol-multi-pos1
   '(let* ([ctc1 (vectorof (vectorof integer?))]
           [ctc2 (vectorof symbol?)]
           [v (contract
               ctc2
               (add-many-contracts
                11
                ctc1 (vector (vector 1)) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "pos")

  (test/spec-failed
   'vector-symbol-multi-pos2
   '(let* ([ctc1 (vectorof (vectorof integer?))]
           [ctc2 (vectorof symbol?)]
           [v (contract ctc2 (add-many-contracts 11 ctc1 (vector 'foo) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-ref v 0))
   "inner-pos")

  (test/spec-failed
   'vector-symbol-multi-neg1
   '(let* ([ctc1 (vectorof symbol?)]
           [ctc2 (vectorof (vectorof integer?))]
           [v (contract
               ctc2
               (add-many-contracts 11 ctc1 (vector 'dont-care) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 (vector 1)))
   "inner-neg")

  (test/spec-failed
   'vector-symbol-multi-neg2
   '(let* ([ctc1 (vectorof symbol?)]
           [ctc2 (vectorof (vectorof integer?))]
           [v (contract
               ctc2
               (add-many-contracts 11 ctc1 (vector 'dont-care) 'inner-pos 'inner-neg) 'pos 'neg)])
      (vector-set! v 0 'foo))
   "neg")

  (test/spec-failed
   'collapse-keep-different-blame
   '(let ()
      (define blame-holder (contract (vectorof (vectorof positive?)) (vector (vector 1)) 'pos #f))
      (define blame-holder2 (contract (vectorof (vectorof negative?)) (vector (vector 1)) 'pos #f))
      (define pos-blame1 (value-blame blame-holder))
      (define pos-blame2 (value-blame blame-holder2))
      (define ctc (vectorof (vectorof positive?)))
      (define ctc2 (vectorof (vectorof negative?)))
      (define lnp (get/build-late-neg-projection ctc))
      (define lnp2 (get/build-late-neg-projection ctc2))
      (define ctc+blame (lnp pos-blame1))
      (define ctc+blame2 (lnp2 pos-blame2))
      (define v (vector (vector -1)))

      (define cv1 (for/fold ([cv v]) ([_ (in-range 3)]) (ctc+blame cv 'neg1)))
      (define cv2 (for/fold ([cv cv1]) ([_ (in-range 3)]) (ctc+blame2 cv 'neg2)))
      (define cv3 (for/fold ([cv cv2]) ([_ (in-range 3)]) (ctc+blame cv 'neg3)))
      (define cv4 (for/fold ([cv cv3]) ([_ (in-range 2)]) (ctc+blame2 cv 'neg4)))

      (define iv (vector-ref cv4 0))
      (vector-set! iv 0 -1))
   "neg3")

  (test/spec-failed
   'collapse-keep-higher-order
   '(let ()
      (define blame-holder (contract (vectorof (vectorof positive?)) (vector (vector 1)) 'pos #f))
      (define pos-blame1 (value-blame blame-holder))
      (define ctc (vectorof (vectorof positive?)))
      (define lnp (get/build-late-neg-projection ctc))
      (define ctc+blame (lnp pos-blame1))
      (define v (vector (vector 1)))

      (define cv1 (for/fold ([cv v]) ([_ (in-range 4)]) (ctc+blame cv 'neg1)))
      (define cv2 (for/fold ([cv cv1]) ([_ (in-range 4)]) (ctc+blame cv 'neg2)))
      (define cv3 (for/fold ([cv cv2]) ([_ (in-range 3)]) (ctc+blame cv 'neg1)))

      (define iv (vector-ref cv3 0))
      (vector-set! iv 0 -1))
   "neg1")

  (test/neg-blame
   'dont-trust-untrustworthy-stronger-implementations
   '(let ()
      (define stronger-any/c
        (make-chaperone-contract #:name 'stronger-any/c
                                 #:late-neg-projection (lambda (blame)
                                                         (lambda (val neg-party)
                                                           val))
                                 #:stronger (lambda (a b) #t)))

      (define (add-ctcs ctc f)
        (for/fold ([f f])
                  ([i (in-range 10)])
          (contract ctc f 'A 'B #f #f)))

      (define ovec (vector (cons 1 2)))

      (define vec
        (contract (vector/c pair?)
                  ovec
                  'pos 'neg))

      (vector-set! (add-ctcs (vector/c stronger-any/c) vec) 0 "bad value")
      ;; need to be sure this doesn't return `"bad value"` but the
      ;; previous line is where the exception gets raised
      (vector-ref ovec 0)))

  )
