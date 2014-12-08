#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-basic-contract-namespace
                                            'racket/contract)])
  (define exn:fail:contract:blame-object 
    (contract-eval 'exn:fail:contract:blame-object))
  (define exn:fail:contract:blame?
    (contract-eval 'exn:fail:contract:blame?))
  
  (contract-error-test
   'assertion1
   #'(begin
       (eval '(module contract-test-suite1 racket/base
                (require racket/contract)
                (invariant-assertion integer? #f)))
       (eval '(require 'contract-test-suite1)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"contract from: contract-test-suite1" (exn-message x)))))
  
  (contract-error-test
   'assertion2
   #'(begin
       (eval '(module contract-test-suite2 racket/base
                (require racket/contract)
                (invariant-assertion integer? #f)))
       (eval '(require 'contract-test-suite2)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"blaming: contract-test-suite2" (exn-message x)))))
  
  (contract-error-test
   'assertion3
   #'(begin
       (eval '(module contract-test-suite3 racket/base
                (require racket/contract)
                ((invariant-assertion (-> integer? integer?) values) #f)))
       (eval '(require 'contract-test-suite3)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"blaming: contract-test-suite3" (exn-message x))))))
