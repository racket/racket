#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/pos-blame
   'or/c1
   '(contract (or/c false/c) #t 'pos 'neg))
  
  (test/spec-passed
   'or/c2
   '(contract (or/c false/c) #f 'pos 'neg))
  
  (test/spec-passed
   'or/c3
   '((contract (or/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))
  
  (test/neg-blame
   'or/c4
   '((contract (or/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) #f))
  
  (test/pos-blame
   'or/c5
   '((contract (or/c (-> integer? integer?)) (lambda (x) #f) 'pos 'neg) 1))
  
  (test/spec-passed
   'or/c6
   '(contract (or/c false/c (-> integer? integer?)) #f 'pos 'neg))
  
  (test/spec-passed
   'or/c7
   '((contract (or/c false/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))
  
  (test/spec-passed/result
   'or/c8
   '((contract ((or/c false/c (-> string?))  . -> . any)
               (λ (y) y)
               'pos
               'neg)
     #f)
   #f)
  
  (test/spec-passed/result
   'or/c9
   '((contract (or/c (-> string?) (-> integer? integer?))
               (λ () "x")
               'pos
               'neg))
   "x")
  
  (test/spec-passed/result
   'or/c10
   '((contract (or/c (-> string?) (-> integer? integer?))
               (λ (x) x)
               'pos
               'neg)
     1)
   1)
  
  (test/pos-blame
   'or/c11
   '(contract (or/c (-> string?) (-> integer? integer?))
              1
              'pos
              'neg))
  
  (test/pos-blame
   'or/c12
   '((contract (or/c (-> string?) (-> integer? integer?))
               1
               'pos
               'neg)
     'x))
  
  (test/pos-blame
   'or/c13
   '(contract (or/c not) #t 'pos 'neg))
  
  (test/spec-passed
   'or/c14
   '(contract (or/c not) #f 'pos 'neg))
  
  (test/spec-passed/result
   'or/c-not-error-early
   '(begin (or/c (-> integer? integer?) (-> boolean? boolean?))
           1)
   1)
  
  (contract-error-test
   'contract-error-test4
   #'(contract (or/c (-> integer? integer?) (-> boolean? boolean?))
               (λ (x) x)
               'pos
               'neg)
   exn:fail?)
  
  (test/spec-passed/result
   'or/c-ordering
   '(let ([x '()])
      (contract (or/c (lambda (y) (set! x (cons 2 x)) #f) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(1 2))
  
  (test/spec-passed/result
   'or/c-ordering2
   '(let ([x '()])
      (contract (or/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(2))
  
  (test/spec-passed
   'or/c-hmm
   '(let ([funny/c (or/c (and/c procedure? (-> any)) (listof (-> number?)))])
      (contract (-> funny/c any) void 'pos 'neg)))
  
  
  (test/spec-passed
   'or/c-opt-unknown-flat
   '(let ()
      (define arr (-> number? number?))
      ((contract (opt/c (or/c not arr)) (λ (x) x) 'pos 'neg) 1)))
  
  
  
  
  
  
  ;
  ;
  ;
  ;                          ;;;;  ;;
  ;                          ;;;;  ;;
  ;  ;;;;;;;  ;;;; ;;;    ;;;;;;;  ;;  ;;;;;
  ;  ;;;;;;;; ;;;;;;;;;  ;;;;;;;;  ;; ;;;;;;
  ;      ;;;; ;;;; ;;;; ;;;;;;;;;  ;;;;;;;;;
  ;   ;;;;;;; ;;;; ;;;; ;;;; ;;;; ;; ;;;;
  ;  ;;  ;;;; ;;;; ;;;; ;;;;;;;;; ;; ;;;;;;;
  ;  ;;;;;;;; ;;;; ;;;;  ;;;;;;;; ;;  ;;;;;;
  ;   ;; ;;;; ;;;; ;;;;   ;;;;;;; ;;   ;;;;;
  ;                               ;;
  ;
  ;
  
  (test/spec-passed
   'and/c1
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'and/c2
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) x)
               'pos
               'neg)
     200))
  
  (test/pos-blame
   'and/c3
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) 200)
               'pos
               'neg)
     1))
  
  
  
  
  (test/spec-passed/result
   'and/c-ordering
   '(let ([x '()])
      (contract (and/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(1 2))
  
  (test/spec-passed/result
   'ho-and/c-ordering
   '(let ([x '()])
      ((contract (and/c (-> (lambda (y) (set! x (cons 1 x)) #t)
                            (lambda (y) (set! x (cons 2 x)) #t))
                        (-> (lambda (y) (set! x (cons 3 x)) #t)
                            (lambda (y) (set! x (cons 4 x)) #t)))
                 (λ (x) x)
                 'pos
                 'neg)
       1)
      (reverse x))
   '(3 1 2 4))
  
  (test/spec-passed/result
   'and/c-isnt
   '(and (regexp-match #rx"isn't: even?"
                       (with-handlers ((exn:fail? exn-message))
                         (contract (and/c integer? even? positive?)
                                   -3
                                   'pos
                                   'neg)
                         "not the error!"))
         #t)
   #t)
  
  (test/spec-passed
   'contract-flat1
   '(contract not #f 'pos 'neg))
  
  (test/pos-blame
   'contract-flat2
   '(contract not #t 'pos 'neg))
  
  
  (test/neg-blame
   'ho-or/c-val-first1
   '((contract (-> (or/c (-> number?)
                         (-> number? number?))
                   number?)
               (λ (x) 1)
               'pos 'neg)
     (lambda (x y z) 1)))
  
  (test/neg-blame
   'ho-or/c-val-first2
   '((contract (-> (or/c (-> number? number?)
                         (-> number? number?))
                   number?)
               (λ (x) 1)
               'pos 'neg)
     (lambda (x) 1))))
