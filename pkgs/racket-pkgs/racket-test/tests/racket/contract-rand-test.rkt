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
(check-not-exn (λ () (test-contract-generation (or/c boolean? boolean?))))
(check-not-exn (λ () (test-contract-generation any/c)))

(check-not-exn (λ () (test-contract-generation (listof boolean?))))
(check-not-exn (λ () (test-contract-generation (listof some-crazy-predicate?))))
(check-not-exn (λ () (test-contract-generation (non-empty-listof boolean?))))
(check-not-exn (λ () (test-contract-generation (list/c boolean? number?))))
(check-not-exn (λ () ((car (test-contract-generation (list/c (-> number? number?)))) 0)))

(check-exn exn:fail? (λ () ((test-contract-generation (-> char? integer?)) 0)))
(check-not-exn (λ () ((test-contract-generation (-> integer? integer?)) 1)))
(check-not-exn (λ () ((test-contract-generation (-> (-> integer? integer?) boolean?)) +)))
(check-not-exn 
 (λ () ((test-contract-generation (-> some-crazy-predicate? some-crazy-predicate?)) 11)))
(check-not-exn 
 (λ () (((test-contract-generation (-> (-> (>/c 10) (>/c 10))))) 11)))


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
    [(_ N pred exp)
     (syntax/loc stx
       (check-pred
        pred
        (with-handlers ([exn:fail? exn-message])
          (contract-exercise exp N)
          (void))))]))


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
