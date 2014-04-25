#lang racket/base

(require racket/contract
         racket/contract/private/generate-base
         rackunit
         rackunit/text-ui
         net/url)

;; this is expected to never have a generator.
(define (some-crazy-predicate? x) (and (number? x) (= x 11)))

(define (test-contract-generation ctc #:size [size 10])
  (define example-vals (contract-random-generate ctc size))
  (contract ctc example-vals 'pos 'neg))

(for ([(k v) (in-hash predicate-generator-table)])
  (check-not-exn (λ () (test-contract-generation k))))

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
(check-not-exn (λ () (test-contract-generation (or/c boolean? boolean?))))

(check-not-exn (λ () (test-contract-generation (listof boolean?))))
(check-not-exn (λ () (test-contract-generation (listof some-crazy-predicate?))))
(check-not-exn (λ () (test-contract-generation (list/c boolean? number?))))
(check-not-exn (λ () ((car (test-contract-generation (list/c (-> number? number?)))) 0)))

(check-exn exn:fail? (λ () ((test-contract-generation (-> char? integer?)) 0)))
(check-not-exn (λ () ((test-contract-generation (-> integer? integer?)) 1)))
(check-not-exn (λ () ((test-contract-generation (-> (-> integer? integer?) boolean?)) +)))

(define (cannot-generate-exn? x)
  (and (exn:fail? x)
       (regexp-match #rx"contract-random-generate: unable to construct" 
                     (exn-message x))))
(check-exn cannot-generate-exn? (λ () (test-contract-generation some-crazy-predicate?)))
(check-exn cannot-generate-exn? (λ () (test-contract-generation (list/c some-crazy-predicate?))))
