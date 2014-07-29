#lang racket/base

(require racket/contract
         racket/contract/private/generate-base
         rackunit
         (for-syntax racket/base))

;; this is expected to never have a generator.
(define (some-crazy-predicate? x) (and (number? x) (= x 11)))

(define (test-contract-generation ctc #:size [size 10])
  (define example-val (contract-random-generate ctc size))
  (contract ctc example-val 'pos 'neg))

(for ([(k v) (in-hash predicate-generator-table)])
  (check-not-exn (λ () (test-contract-generation k))))

(check-not-exn (λ () (test-contract-generation natural-number/c)))

;; test =, eq?, and equal? contract random generators
(check-not-exn (λ () (test-contract-generation 0)))
(check-not-exn (λ () (test-contract-generation 'x)))
(check-not-exn (λ () (test-contract-generation "x")))

(check-not-exn (λ () (test-contract-generation (listof boolean?))))
(check-not-exn (λ () (test-contract-generation (listof number?))))

(check-not-exn (λ () (test-contract-generation (between/c 1 100))))
(check-not-exn (λ () (test-contract-generation (between/c 1.0 100.0))))
(check-not-exn (λ () (test-contract-generation (listof integer?))))
(check-not-exn (λ () (test-contract-generation (>=/c 0))))
(check-not-exn (λ () (test-contract-generation (>=/c 0.0))))
(check-not-exn (λ () (test-contract-generation (<=/c 0))))
(check-not-exn (λ () (test-contract-generation (<=/c 0.0))))
(check-not-exn (λ () (test-contract-generation (>/c 0))))
(check-not-exn (λ () (test-contract-generation (>/c 0.0))))
(check-not-exn (λ () (test-contract-generation (</c 0))))
(check-not-exn (λ () (test-contract-generation (</c 0.0))))
(check-not-exn (λ () (test-contract-generation (=/c 0))))
(check-not-exn (λ () (test-contract-generation (=/c 0.0))))
(check-not-exn (λ () (test-contract-generation (and/c real? (not/c negative?)))))
(check-not-exn (λ () (test-contract-generation (and/c rational? (not/c negative?)))))
(check-not-exn (λ () (test-contract-generation (or/c boolean? boolean?))))
(check-not-exn (λ () (test-contract-generation (cons/c integer? boolean?))))
(check-not-exn (λ () (test-contract-generation any/c)))

(check-not-exn (λ () (test-contract-generation (listof boolean?))))
(check-not-exn (λ () (test-contract-generation (listof some-crazy-predicate?))))
(check-not-exn (λ () (test-contract-generation (non-empty-listof boolean?))))
(check-not-exn (λ () (test-contract-generation (list/c boolean? number?))))
(check-not-exn (λ () ((car (test-contract-generation (list/c (-> number? number?)))) 0)))

(check-not-exn
 (λ ()
   (test-contract-generation
    (flat-rec-contract
     even-length-list/c
     (or/c (cons/c any/c (cons/c any/c even-length-list/c))
           '())))))

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

(check-exn exn:fail? (λ () ((test-contract-generation (-> char? integer?)) 0)))
(check-not-exn (λ () ((test-contract-generation (-> integer? integer?)) 1)))
(check-not-exn (λ () ((test-contract-generation (-> integer? any)) 1)))
(check-not-exn (λ () ((test-contract-generation (-> integer? (-> integer? any))) 1)))
(check-not-exn (λ () ((test-contract-generation (-> (-> integer? any) integer?))
                      (λ (i) (values 1 2 3)))))
(check-not-exn (λ () ((test-contract-generation (-> (-> integer? integer?) boolean?)) +)))
(check-not-exn 
 (λ () ((test-contract-generation (-> some-crazy-predicate? some-crazy-predicate?)) 11)))
(check-not-exn 
 (λ () (((test-contract-generation (-> (-> (>/c 10) (>/c 10))))) 11)))

(check-not-exn
 (λ ()
   (define f (test-contract-generation (let ([α (new-∀/c)]) (-> α α))))
   (f 4)
   (define ans (f 5))
   (unless (or (equal? ans 4) (equal? ans 5))
     (error 'polymorphism-problem!))))

(define (cannot-generate-exn? x)
  (and (exn:fail? x)
       (regexp-match #rx"contract-random-generate: unable to construct" 
                     (exn-message x))))
(check-exn cannot-generate-exn? (λ () (test-contract-generation some-crazy-predicate?)))
(check-exn cannot-generate-exn? (λ () (test-contract-generation (list/c some-crazy-predicate?))))


(check-not-exn (lambda () (test-contract-generation (or/c #f number?))))
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
(check-exn cannot-generate-exn? (λ () (test-contract-generation 
                                       (or/c some-crazy-predicate?
                                             some-crazy-predicate?))))

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
 (contract (-> (or/c #f some-crazy-predicate?) (or/c #f some-crazy-predicate?))
           (λ (x) (if x 'fail 11))
           'pos
           'neg))
