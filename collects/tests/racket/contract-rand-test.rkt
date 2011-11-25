#lang racket/base

(require racket/contract
         rackunit
         rackunit/text-ui
         net/url)

(define (test-contract-generation ctc 
                                  [monkey-with values] 
                                  #:size [size 10])
  (define example-vals (contract-random-generate ctc size))
  (monkey-with (contract ctc example-vals 'pos 'neg)))

(define pred-tests
  (test-suite
   "Predicate contract"
   (check-not-exn (λ () (test-contract-generation integer?)))
   (check-not-exn (λ () (test-contract-generation exact-nonnegative-integer?)))
   (check-not-exn (λ () (test-contract-generation boolean?)))
   (check-not-exn (λ () (test-contract-generation char?)))
   (check-not-exn (λ () (test-contract-generation byte?)))
   (check-not-exn (λ () (test-contract-generation bytes?)))
   (check-not-exn (λ () (test-contract-generation string?)))
  ))

(define flat-ctc-tests
  (test-suite
    "Built-in flat contracts"
    (check-not-exn (λ () (test-contract-generation (between/c 1 100))))
    (check-not-exn (λ () (test-contract-generation (listof integer?))))
    (check-not-exn (λ () (test-contract-generation (>=/c 0))))
    (check-not-exn (λ () (test-contract-generation (<=/c 0))))
    (check-not-exn (λ () (test-contract-generation (>/c 0))))
    (check-not-exn (λ () (test-contract-generation (</c 0))))
    (check-not-exn (λ () (test-contract-generation (or/c boolean? boolean?))))
    ))

(define func-tests
  (test-suite
    "Function contracts"
    (check-exn exn:fail? (λ ()
                            ((test-contract-generation (-> char?
                                                           integer?)) 0)))
    (check-not-exn (λ () ((test-contract-generation (-> integer?
                                                        integer?)) 1)))
    (check-not-exn (λ () ((test-contract-generation 
                             (-> (-> integer? 
                                     integer?)
                                 boolean?)) 
                            +)))))

(define ctc-gen-tests
  (test-suite
    "All random contract generation tests"
    pred-tests
    flat-ctc-tests
    func-tests))


(run-tests ctc-gen-tests)
