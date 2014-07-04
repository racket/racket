#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-basic-contract-namespace 'racket/list)])
  
  (test/spec-passed/result
   'list-contract-1
   '(list-contract? 1)
   #f)
  
  (test/spec-passed/result
   'list-contract-2
   '(list-contract? (λ (a b c) a)) ;; somethign that's not coerceable to a contract
   #f)
  
  (test/spec-passed/result
   'list-contract-3
   '(list-contract? '())
   #t)
  
  (test/spec-passed/result
   'list-contract-4
   '(list-contract? null?)
   #t)
  
  (test/spec-passed/result
   'list-contract-5
   '(list-contract? empty?)
   #t)
  
  (test/spec-passed/result
   'list-contract-6
   '(list-contract? boolean?)
   #f)
  
  (test/spec-passed/result
   'list-contract-7
   '(list-contract? any/c)
   #f)
  
  (test/spec-passed/result
   'list-contract-8
   '(list-contract? (cons/c 1 empty?))
   #t)
  
  (test/spec-passed/result
   'list-contract-9
   '(list-contract? (cons/c 1 2))
   #f)
  
  (test/spec-passed/result
   'list-contract-10
   '(list-contract? (listof any/c))
   #t)
  
  (test/spec-passed/result
   'list-contract-11
   '(list-contract? (non-empty-listof any/c))
   #t)
  
  (test/spec-passed/result
   'list-contract-12
   '(list-contract? (list/c 1 2 3))
   #t)
  
  (test/spec-passed/result
   'list-contract-13
   '(list-contract? (or/c (cons/c 1 empty?) empty?))
   #t)
  
  (test/spec-passed/result
   'list-contract-14
   '(list-contract? (or/c (cons/c (-> integer? integer?) empty?)
                          empty?))
   #t)
  
  (test/spec-passed/result
   'list-contract-15
   '(list-contract? (or/c (cons/c (-> integer? integer?) empty?)
                          (cons/c (-> integer? integer? integer?) empty?)
                          empty?))
   #t)
  
  (test/spec-passed/result
   'list-contract-16
   '(list-contract? 
     (letrec ([c (recursive-contract (or/c (cons/c 1 c) empty?))])
       c))
   #f)
  
  (test/spec-passed/result
   'list-contract-17
   '(list-contract? 
     (letrec ([c (recursive-contract (or/c (cons/c 1 c) empty?) #:list-contract?)])
       c))
   #t)
  
  (test/pos-blame
   'test-contract-18
   '(contract (letrec ([c (recursive-contract (or/c (cons/c any/c c) empty?) 
                                              #:list-contract?)])
                c)
              (read (open-input-string "#1=(1 . #1#)"))
              'pos 'neg))
  
  
  (contract-error-test
   'test-contract-19
   '(contract (recursive-contract 1 #:list-contract?)
              1
              'pos 'neg)
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"list-contract[?]" (exn-message x))))))

