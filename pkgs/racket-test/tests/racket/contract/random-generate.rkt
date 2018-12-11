#lang racket/base

(require racket/contract
         racket/contract/private/generate-base
         (only-in racket/list empty? cons?)
         rackunit
         racket/math
         (for-syntax racket/base))

;; this is expected to never have a generator.
(define (some-crazy-predicate? x) (and (number? x) (= x 11)))

(define (test-contract-generation ctc #:size [size 10])
  (let/ec k
    (define example-val (contract-random-generate 
                         ctc size
                         (λ (no-generator?)
                           (if no-generator?
                               (error 'test-contract-generation
                                      "unable to construct any generator for contract: ~e"
                                      ctc)
                               (k 'we-tried-but-could-not-generate-anything)))))
    (contract ctc example-val 'pos 'neg)))

(for ([(k v) (in-hash predicate-generator-table)])
  (check-not-exn (λ () (test-contract-generation k))))

(check-not-exn (λ () (test-contract-generation natural-number/c)))

;; test =, eq?, and equal? contract random generators
(check-not-exn (λ () (test-contract-generation 1/2)))
(check-not-exn (λ () (test-contract-generation 1/3)))
(check-not-exn (λ () (test-contract-generation 0)))
(check-not-exn (λ () (test-contract-generation 1)))
(check-not-exn (λ () (test-contract-generation 1.0)))
(check-not-exn (λ () (test-contract-generation (expt 10 200))))
(check-not-exn (λ () (test-contract-generation +nan.0)))
(check-not-exn (λ () (test-contract-generation 'x)))
(check-not-exn (λ () (test-contract-generation "x")))
(check-not-exn (λ () (test-contract-generation #t)))

(check-not-exn (λ () (test-contract-generation (listof boolean?))))
(check-not-exn (λ () (test-contract-generation (listof number?))))

(check-not-exn (λ () (test-contract-generation exact-nonnegative-integer?)))
(check-not-exn (λ () (test-contract-generation natural?)))
(check-not-exn (λ () (test-contract-generation symbol?)))
(check-not-exn (λ () (test-contract-generation (integer-in 0 100))))
(check-not-exn (λ () (test-contract-generation (integer-in 0 (expt 2 1000)))))
(check-not-exn (λ () (test-contract-generation (integer-in 0 #f))))
(check-not-exn (λ () (test-contract-generation (integer-in #f 0))))
(check-not-exn (λ () (test-contract-generation (integer-in #f #f))))
(check-not-exn (λ () (test-contract-generation (char-in #\a #\z))))
(check-not-exn (λ () (test-contract-generation #\a)))
(check-not-exn (λ () (test-contract-generation (between/c 1 100))))
(check-not-exn (λ () (test-contract-generation (between/c 1.0 100.0))))
(check-not-exn (λ () (test-contract-generation (listof integer?))))
(check-not-exn (λ () (test-contract-generation (>=/c 0))))
(check-not-exn (λ () (test-contract-generation (>=/c 0.0))))
(check-not-exn (λ () (test-contract-generation (<=/c 0))))
(check-not-exn (λ () (test-contract-generation (<=/c 0.0))))
(check-not-exn (λ () (test-contract-generation (>/c 0))))
(check-not-exn (λ () (test-contract-generation (>/c 0.0))))
(check-not-exn (λ () (test-contract-generation (>/c -inf.0))))
(check-not-exn (λ () (test-contract-generation (</c 0))))
(check-not-exn (λ () (test-contract-generation (</c 0.0))))
(check-not-exn (λ () (test-contract-generation (</c +inf.0))))
(check-not-exn (λ () (test-contract-generation (=/c 0))))
(check-not-exn (λ () (test-contract-generation (=/c 0.0))))
(check-not-exn (λ () (test-contract-generation (or/c boolean? boolean?))))
(check-not-exn (λ () (test-contract-generation (first-or/c boolean? boolean?))))
(check-not-exn (λ () (test-contract-generation (cons/c integer? boolean?))))
(check-not-exn (λ () (test-contract-generation (cons/dc [hd integer?] [tl (hd) (<=/c hd)]))))
(check-not-exn (λ () (test-contract-generation (cons/dc [hd (tl) (<=/c tl)] [tl integer?]))))
(check-not-exn (λ () (test-contract-generation any/c)))

(check-not-exn (λ () (test-contract-generation (and/c real? (not/c negative?)))))
(check-not-exn (λ () (test-contract-generation (and/c rational? (not/c negative?)))))
(check-not-exn (λ () (test-contract-generation (and/c integer? even?))))
(check-not-exn (λ () (test-contract-generation (and/c procedure? (-> integer? integer?)))))
(check-not-exn (λ () (test-contract-generation (and/c integer? even?))))
(check-not-exn (λ () (test-contract-generation (or/c (and/c real? positive? (</c 0)) boolean?))))
(check-not-exn (λ () (test-contract-generation (first-or/c (and/c real? positive? (</c 0)) boolean?))))

(check-not-exn (λ () (test-contract-generation (listof boolean?))))
(check-not-exn (λ () (test-contract-generation (listof some-crazy-predicate?))))
(check-not-exn (λ () (test-contract-generation (non-empty-listof boolean?))))
(check-not-exn (λ () (test-contract-generation (list*of boolean?))))
(check-not-exn (λ () (test-contract-generation (list*of boolean? char?))))
(check-not-exn (λ () (test-contract-generation (list/c boolean? number?))))
(check-not-exn (λ () ((car (test-contract-generation (list/c (-> number? number?)))) 0)))
(check-not-exn (λ () (test-contract-generation (*list/c boolean? number? char?))))
(check-not-exn (λ () (test-contract-generation (-> (*list/c boolean? number? char?) any))))

(check-not-exn (λ () (test-contract-generation (hash/c boolean? boolean?))))
(check-not-exn (λ () (test-contract-generation (hash/c char? integer?))))
(check-not-exn (λ () (test-contract-generation (hash/c string? integer?))))
(check-not-exn (λ () (test-contract-generation (hash/c string? (-> number? boolean?)))))
(check-not-exn (λ () (test-contract-generation (hash/c string? (hash/c integer? string?)))))
(check-not-exn (λ () (test-contract-generation (hash/c (hash/c string? integer?) (hash/c integer? string?)))))

(define hash/c-list
  (for/list ([i (in-range 100)])
    (contract-random-generate
     (hash/c integer? integer?))))

;; hash/c should periodically generate empty hashes
(check-pred
 (λ (v) (not (empty? v)))
 (filter hash-empty? hash/c-list))

;; hash/c should periodically generate hashes with multiple elements
(check-pred
 (λ (v) (not (empty? v)))
 (filter (λ (h) (> (length (hash-values h)) 1)) hash/c-list))

(check-not-exn
 (λ ()
   (test-contract-generation
    (flat-rec-contract
     even-length-list/c
     (or/c (cons/c any/c (cons/c any/c even-length-list/c))
           '())))))

(check-not-exn
 (λ ()
   (test-contract-generation
    (flat-rec-contract
     even-length-list/c
     (first-or/c (cons/c any/c (cons/c any/c even-length-list/c))
            '())))))

(check-not-exn
 (λ ()
   (test-contract-generation
    null?)))

(check-not-exn
 (λ ()
   (test-contract-generation
    empty?)))

(check-not-exn
 (λ ()
   (test-contract-generation
    pair?)))

(check-not-exn
 (λ ()
   (test-contract-generation
    cons?)))

(check-not-exn
 (λ ()
   (test-contract-generation
    (letrec ([c (or/c null? (cons/c real? (recursive-contract c)))])
      c))))
(check-not-exn
 (λ ()
   (test-contract-generation
    (letrec ([c (first-or/c null? (cons/c real? (recursive-contract c)))])
      c))))

(check-not-exn
 (λ ()
   (struct s (a b) #:transparent)
   (test-contract-generation
    (struct/dc s [a integer?] [b boolean?]))))

(check-not-exn
 (λ ()
   (struct s (a b) #:transparent)
   (test-contract-generation
    (struct/c s integer? boolean?))))

(check-not-exn
 (λ ()
   (struct node (v l r) #:transparent)
   (test-contract-generation
    (flat-rec-contract
     tree/c
     (or/c (struct/dc node
                      [v integer?]
                      [l tree/c]
                      [r tree/c])
           #f)))))
(check-not-exn
 (λ ()
   (struct node (v l r) #:transparent)
   (test-contract-generation
    (flat-rec-contract
     tree/c
     (first-or/c (struct/dc node
                       [v integer?]
                       [l tree/c]
                       [r tree/c])
            #f)))))

(check-exn exn:fail? (λ () ((test-contract-generation (-> char? integer?)) 0)))
(check-not-exn (λ () ((test-contract-generation (-> integer? integer?)) 1)))
(check-not-exn (λ () ((test-contract-generation (-> any/c (-> any) any)) 0 void)))
(check-not-exn (λ () ((test-contract-generation (-> integer? any)) 1)))
(check-not-exn (λ () ((test-contract-generation (-> integer? (-> integer? any))) 1)))
(check-not-exn (λ () ((test-contract-generation (-> (-> integer? any) integer?))
                      (λ (i) (values 1 2 3)))))
(check-not-exn (λ () ((test-contract-generation (-> (-> integer? integer?) boolean?)) +)))
(check-not-exn 
 (λ () ((test-contract-generation (-> some-crazy-predicate? some-crazy-predicate?)) 11)))
(check-not-exn 
 (λ () (((test-contract-generation (-> (-> (>/c 10) (>/c 10))))) 11)))
(check-not-exn (λ () ((test-contract-generation (-> any/c any)) 1)))

(check-not-exn
 (λ ()
   (define f (test-contract-generation (let ([α (new-∀/c)]) (-> α α))))
   (f 4)
   (define ans (f 5))
   (unless (or (equal? ans 4) (equal? ans 5))
     (error 'polymorphism-problem!))))

(define (cannot-generate-exn? x)
  (and (exn:fail? x)
       (regexp-match #rx"test-contract-generation: unable to construct" 
                     (exn-message x))))
(check-exn cannot-generate-exn? (λ () (test-contract-generation some-crazy-predicate?)))
(check-exn cannot-generate-exn? (λ () (test-contract-generation (list/c some-crazy-predicate?))))


(check-not-exn (lambda () (test-contract-generation (or/c #f number?))))
(check-not-exn (lambda () (test-contract-generation (first-or/c #f number?))))
(check-not-exn (lambda () (test-contract-generation (or/c some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          some-crazy-predicate?
                                                          #f))))
(check-not-exn (lambda () (test-contract-generation (first-or/c some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           some-crazy-predicate?
                                                           #f))))
(check-exn cannot-generate-exn? (λ () (test-contract-generation 
                                       (or/c some-crazy-predicate?
                                             some-crazy-predicate?))))
(check-exn cannot-generate-exn? (λ () (test-contract-generation 
                                       (first-or/c some-crazy-predicate?
                                              some-crazy-predicate?))))

;; testing a bunch of impossible and/c's inside some or/c doesn't crash
(check-not-exn (λ () (test-contract-generation 
                      (or/c (or/c (and/c integer? boolean?)
                                  (and/c (listof integer?) string?))
                            (and/c (-> number? number?)
                                   any/c
                                   number?)))))
(check-not-exn (λ () (test-contract-generation 
                      (first-or/c (first-or/c (and/c integer? boolean?)
                                    (and/c (listof integer?) string?))
                            (and/c (-> number? number?)
                                   any/c
                                   number?)))))

;; in this test, the and/c shoudl generate a dynamic
;; failure, which should trigger the 'cons/c' failing
;; it shouldn't make a pair of a strange value and #t
(check-not-exn
 (λ ()
   (test-contract-generation
    (cons/c #t (and/c integer? even? odd?)))))

(check-not-exn 
 (λ () 
   (define eleven
     ((test-contract-generation (-> (-> some-crazy-predicate?) some-crazy-predicate?))
      (λ () 11)))
   (unless (= eleven 11)
     (error 'contract-rand-test.rkt "expected 11 got ~s" eleven))))

(check-not-exn 
 (λ () 
   (define eleven
     ((test-contract-generation (-> (-> number? boolean? some-crazy-predicate?)
                                    some-crazy-predicate?))
      (λ (n b) 11)))
   (unless (= eleven 11)
     (error 'contract-rand-test.rkt "expected 11 got ~s" eleven))))

(check-not-exn 
 (λ () 
   (define eleven
     ((test-contract-generation (-> (non-empty-listof some-crazy-predicate?)
                                    some-crazy-predicate?))
      (list 11)))
   (unless (= eleven 11)
     (error 'contract-rand-test.rkt "expected 11 got ~s" eleven))))

(check-exn cannot-generate-exn? (λ () (test-contract-generation 
                                       (-> (listof some-crazy-predicate?)
                                           some-crazy-predicate?))))

(define (pos-exn-or-silence? val-or-exn)
  (or (void? val-or-exn)
      (and (string? val-or-exn)
           (regexp-match #rx"blaming: pos" val-or-exn))))

(define (pos-exn? val-or-exn)
  (and (string? val-or-exn)
       (regexp-match #rx"blaming: pos" val-or-exn)))

(define-syntax (check-exercise stx)
  (syntax-case stx ()
    [(_ N pred . exps)
     (syntax/loc stx
       (check-pred
        pred
        (with-handlers ([exn:fail? exn-message])
          (contract-exercise #:fuel N . exps)
          (void))))]))

(check-exercise
 10
 pos-exn-or-silence?
 (contract (-> #:b boolean? any/c)
           (λ (#:b b) b)
           'pos
           'neg))

(check-exercise
 1
 pos-exn?
 (contract (-> some-crazy-predicate? string?)
           (λ (x) 'not-a-string)
           'pos
           'neg)
 (contract (-> some-crazy-predicate?)
           (λ () 11)
           'wrong-one
           'wrong-two))

(check-exercise
 1
 pos-exn?
 (contract (-> some-crazy-predicate?)
           (λ () 11)
           'wrong-one
           'wrong-two)
 (contract (-> some-crazy-predicate? string?)
           (λ (x) 'not-a-string)
           'pos
           'neg))

(check-exercise
 1
 pos-exn?
 (contract (->i ([i integer?] [b boolean?]) [result string?])
           (λ (i b) 'not-a-string)
           'pos 'neg))

(check-exercise
 1
 pos-exn?
 (contract (->i ([i integer?] [b boolean?]) [result number?] #:post (result) (zero? result))
           (λ (i b) 11)
           'pos 'neg))

;; the tests below that use pos-exn? have a
;; (vanishingly small) probability of not passing. 

(check-exercise
 10000
 pos-exn?
 (contract (-> (or/c #f some-crazy-predicate?) some-crazy-predicate?)
           (λ (x) (if x 'fail 11))
           'pos
           'neg))
(check-exercise
 10000
 pos-exn?
 (contract (-> (first-or/c #f some-crazy-predicate?) some-crazy-predicate?)
           (λ (x) (if x 'fail 11))
           'pos
           'neg))

(check-exercise
 10000
 pos-exn?
 (contract (-> (or/c #f some-crazy-predicate?) (or/c #f some-crazy-predicate?))
           (λ (x) (if x 'fail 11))
           'pos
           'neg))
(check-exercise
 10000
 pos-exn?
 (contract (-> (first-or/c #f some-crazy-predicate?) (first-or/c #f some-crazy-predicate?))
           (λ (x) (if x 'fail 11))
           'pos
           'neg))

(let () ;; test generate / exercise for `build-flat-contract-property contracts
  (define even-list/c
    (let ()
      (struct ctc ()
        #:property
        prop:flat-contract
        (build-flat-contract-property
         #:name (λ (c) 'even-list/c)
         #:first-order (λ (c) (λ (v) (and (list? v) (andmap even? v))))
         #:late-neg-projection
         (λ (c)
           (λ (b)
             (λ (v neg-party)
               (unless (and (list? v) (andmap even? v))
                 (raise-blame-error b v
                                    #:missing-party neg-party
                                    "expected even list, got ~v" v))
               (map values v))))))
      (ctc)))
  (define even-list/c/generate
    (let ()
      (struct ctc ()
        #:property
        prop:flat-contract
        (build-flat-contract-property
         #:name (λ (c) 'even-list/c)
         #:first-order (λ (c) (λ (v) (and (list? v) (andmap even? v))))
         #:late-neg-projection
         (λ (c)
           (λ (b)
             (λ (v neg-party)
               (unless (and (list? v) (andmap even? v))
                 (raise-blame-error b v
                                    #:missing-party neg-party
                                    "expected even list, got ~v" v))
               (map values v))))
         #:generate
         (λ (c)
           (λ (fuel)
             (λ () '(2))))))
      (ctc)))
  (check-exn cannot-generate-exn? (λ () (test-contract-generation even-list/c)))
  (check-not-exn (λ () (test-contract-generation even-list/c/generate)))
  (check-exercise 2 void? even-list/c)
  (check-exercise 2 void? even-list/c/generate))

(let () ;; test the default value of generate / exercise for make-chaperone-contract
  (define custom-any/c
    (make-chaperone-contract
     #:late-neg-projection
     (λ (b) (λ (v np) v))))
  (define any->any/c
    (make-chaperone-contract
     #:late-neg-projection
     (λ (b)
       (λ (v np)
         (chaperone-procedure v values impersonator-prop:contracted any->any/c)))))
  (define/contract proc any->any/c values)
  (check-exn cannot-generate-exn? (λ () (test-contract-generation custom-any/c)))
  (check-not-exn (λ () (contract-exercise proc))))

(let ()
  (struct impersonate-any/c-struct ()
    #:property prop:contract
    (build-contract-property
     #:late-neg-projection
     (λ (ctc) (λ (b) (λ (v np) v)))))
  (define impersonate-any/c (impersonate-any/c-struct))
  (struct chaperone-proc?/c-struct ()
    #:property prop:chaperone-contract
    (build-chaperone-contract-property
     #:late-neg-projection
     (λ (ctc) (λ (b) (λ (v np) v)))))
  (define chaperone-proc?/c (chaperone-proc?/c-struct))
  (check-exn cannot-generate-exn? (λ () (test-contract-generation impersonate-any/c)))
  (check-exn cannot-generate-exn?
             (λ ()
               (test-contract-generation
                (->i ([n integer?])
                     [_ (n) (λ (r) (eq? r (even? n)))]))))
  ;; Testing the default return value for contract-struct-generate
  (check-exn cannot-generate-exn? (λ () (test-contract-generation chaperone-proc?/c))))

(check-exercise
 10
 pos-exn?
 (contract (-> integer? (-> integer? integer?))
           (λ (x) (λ (y) #f))
           'pos
           'neg))

(check-exercise
 10
 pos-exn?
 (contract (->i ([m integer?])
                [result (-> integer? integer?)])
           (λ (x) (λ (y) #f))
           'pos
           'neg))

(check-exercise
 5
 void?
 (contract (-> (and/c #f #t) any)
           (λ (_) 'thing)
           'pos
           'neg))

(check-exercise
 10
 pos-exn?
 (contract (hash/c symbol? (-> integer? boolean?))
           (make-hash (list (cons 'lam (λ (n) (+ n 1)))))
           'pos
           'neg))

;; a test for contract-random-generate/choose
(let ()
  (struct make-gen-choose/c ()
    #:property prop:chaperone-contract
    (build-chaperone-contract-property
     #:late-neg-projection
     (λ (ctc) (λ (b) (λ (v np) v)))
     #:generate
     (λ (ctc) (λ (fuel) (contract-random-generate/choose number? 10)))))
  (check-not-exn (λ () (test-contract-generation (make-gen-choose/c)))))
