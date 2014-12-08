#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/unit 'racket/class 'racket/contract)])
  
  (ctest #f value-contract #f)
  (ctest #f value-contract (λ (x) x))
  (ctest #f value-contract (unit (import) (export)))
  (ctest #f value-contract object%)
  
  (contract-eval
   `(let ([ctc (-> number? number?)])
      (,test ctc value-contract (contract ctc (λ (x) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (->* (number?) (number?) number?)])
      (,test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (->d ([x number?]) ([y number?]) [_ number?])])
      (,test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (->i ([x number?]) ([y number?]) [_ number?])])
      (,test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (unconstrained-domain-> number?)])
      (,test ctc value-contract (contract ctc (λ (x) 3) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (case-> (-> number? number? number?) (-> number? number?))])
      (,test ctc value-contract (contract ctc (case-lambda [(x) 3] [(x y) (+ x y)]) 'pos 'neg))))
  
  (contract-eval
   `(let ([ctc (box/c number?)])
      (,test ctc value-contract (contract ctc (box 3) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (hash/c number? number?)])
      (,test ctc value-contract (contract ctc (make-hash) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (vectorof number?)])
      (,test ctc value-contract (contract ctc (vector 1 2 3) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (vector/c number? number?)])
      (,test ctc value-contract (contract ctc (vector 4 5) 'pos 'neg))))
  
  (contract-eval
   `(let ([ctc (object-contract)])
      (,test ctc value-contract (contract ctc (new object%) 'pos 'neg))))
  
  (test/spec-passed/result
   'value-contract
   '(let ()
      (define c (-> integer? integer?))
      (define f (contract c (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so 
      ;; we require only that when there is blame, that the blame is right.
      (or (and (has-contract? f)
               (equal? c (value-contract f)))
          #t))
   #t)
  
  (test/spec-passed/result
   'value-blame
   '(let ()
      (define f
        (contract (-> integer? integer?) (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so 
      ;; we require only that when there is blame, that the blame is right.
      (or (and (has-blame? f)
               (blame-positive (value-blame f)))
          'pos))
   'pos))