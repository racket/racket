#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace 
                                            'racket/contract/private/arrow)])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; procedure accepts-and-more
  ;;
  
  (ctest #t procedure-accepts-and-more? (lambda (x . y) 1) 3)
  (ctest #t procedure-accepts-and-more? (lambda (x . y) 1) 2)
  (ctest #t procedure-accepts-and-more? (lambda (x . y) 1) 1)
  (ctest #f procedure-accepts-and-more? (lambda (x . y) 1) 0)
  
  (ctest #t procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 3)
  (ctest #t procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 2)
  (ctest #t procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 1)
  (ctest #f procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 0)
  
  (ctest #t procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 2)
  (ctest #t procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 1)
  (ctest #f procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 0))
