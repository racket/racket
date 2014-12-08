#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace )])
  (test/spec-passed
   'contract-case->0a
   '(contract (case->)
              (lambda (x) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-case->0b
   '(contract (case->)
              (lambda () 1)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-case->0c
   '(contract (case->)
              1
              'pos
              'neg))
  
  (test/spec-passed
   'contract-case->0d
   '(contract (case->)
              (case-lambda)
              'pos
              'neg))
  
  
  (test/pos-blame
   'contract-case->1
   '(contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
              (lambda (x) x)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-case->2
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1 2))
  
  (test/pos-blame
   'contract-case->3
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-case->4
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     'a 2))
  
  (test/neg-blame
   'contract-case->5
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     2 'a))
  
  (test/neg-blame
   'contract-case->6
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     #t))
  
  (test/pos-blame
   'contract-case->7
   '((contract (case-> (integer? integer? . -> . integer?) (-> integer? #:rest any/c boolean?))
               (lambda x #\a)
               'pos
               'neg)
     1 2))
  
  
  (test/pos-blame
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (-> integer? #:rest any/c boolean?))
               (lambda x #t)
               'pos
               'neg)
     1 2))
  
  (test/spec-passed
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (-> integer? #:rest any/c boolean?))
               (lambda x 1)
               'pos
               'neg)
     1 2))
  
  (test/spec-passed/result
   'contract-case->9
   '((contract (case-> (-> integer? any))
               (lambda (x) 1)
               'pos
               'neg)
     1)
   1)
  
  (test/neg-blame
   'contract-case->10
   '((contract (case-> (-> integer? any))
               (lambda (x) 1)
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-case->11
   '(contract (case-> (-> integer? any) (->  integer? integer? any))
              (lambda (x) 1)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-case->12
   '((contract (case-> (-> integer? any) (-> integer? integer? any))
               (case-lambda [(x) 1] [(x y) 1])
               'pos
               'neg)
     #f))
  
  (test/spec-passed/result
   'contract-case->13
   '((contract (case-> (-> integer? any) (-> integer? integer? any))
               (case-lambda [(x) 1] [(x y) 1])
               'pos
               'neg)
     1)
   1)
  
  (test/spec-passed/result
   'contract-case->14
   '(let ([f
           (contract (case-> (-> char?) (-> integer? boolean?) (-> symbol? input-port? string?))
                     (case-lambda
                       [() #\a]
                       [(x) (= x 0)]
                       [(sym port)
                        (string-append
                         (symbol->string sym)
                         (read port))])
                     'pos
                     'neg)])
      (list (f)
            (f 1)
            (f 'x (open-input-string (format "~s" "string")))))
   (list #\a #f "xstring"))
  
  (test/pos-blame
   'contract-case->15
   '((contract (case-> (-> real? real? real?)
                       (-> real? (values real? real?)))
               (case-lambda
                 [(x y) 1]
                 [(x) 1])
               'pos 'neg)
     1))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   case-> arity checking tests                          ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test/well-formed '(case-> (-> integer? integer?)))
  (test/well-formed '(case-> (-> integer? integer?) (-> integer? integer? integer?)))
  (test/well-formed '(case-> (-> integer? integer?) (-> integer? integer? any)))
  (test/well-formed '(case-> (-> integer? any) (-> integer? integer? any))))