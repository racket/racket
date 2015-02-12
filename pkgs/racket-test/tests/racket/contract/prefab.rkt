#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/spec-passed
   'prefab/flat-1
   '(contract (prefab/c 'foo integer? string?)
              #s(foo 3 "foo")
              'pos
              'neg))

  (test/pos-blame
   'prefab/flat-2
   '(contract (prefab/c 'foo integer? string?)
              #s(foo "foo" 3)
              'pos
              'neg))

  (test/pos-blame
   'prefab/flat-3
   '(contract (prefab/c '(foo 2 bar 0) integer? string?)
              #s(foo "foo" 3)
              'pos
              'neg))

  (test/pos-blame
   'prefab/flat-4
   '(contract (prefab/c 'foo integer? string?)
              #s((foo 2 bar 0) "foo" 3)
              'pos
              'neg))

  (test/spec-passed
   'prefab/flat-5
   '(contract (prefab/c 'foo integer? string?)
              #s((bar 0 foo 2) 3 "foo")
              'pos
              'neg))

  (test/spec-passed
   'prefab/flat-6
   '(contract (prefab/c '(bar 0 (0 "hello") foo 2 (0 #f)) integer? string?)
              #s((bar 0 foo 2) 3 "foo")
              'pos
              'neg))

  (test/pos-blame
   'prefab/flat-7
   '(contract (prefab/c '(foo 2 (1 'def)) integer? string? symbol?)
              #s((foo 2 (0 'def)) "foo" 3)
              'pos
              'neg))

  (test/spec-passed
   'prefab/flat-8
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (contract (prefab/c '(foo #(0)) integer?)
                (foo 3)
                'pos
                'neg)))

  (test/pos-blame
   'prefab/flat-9
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (contract (prefab/c '(foo 1) integer?)
                (foo 3)
                'pos
                'neg)))

  (test/pos-blame
   'prefab/chap-1
   '(contract (prefab/c 'foo (-> string? string?))
              #s(foo "not a function")
              'pos
              'neg))

  (test/neg-blame
   'prefab/chap-2
   '(let ([prefab (contract (prefab/c 'foo (-> string? string?))
                            `#s(foo ,(λ (x) x))
                            'pos
                            'neg)])
     ((vector-ref (struct->vector prefab) 1)
      'not-a-string)))

  (test/spec-passed
   'prefab/chap-3
   '(contract (prefab/c 'foo (-> string? string?))
              `#s(foo ,(λ (x) x))
              'pos
              'neg))

  (test/pos-blame
   'prefab/chap-4
   '(let ([prefab (contract (prefab/c 'foo (-> string? string?))
                            `#s(foo ,(λ (x) 0))
                            'pos
                            'neg)])
     ((vector-ref (struct->vector prefab) 1) "foo"))) 

  (test/neg-blame
   'prefab/chap-5
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (define a-foo (contract (prefab/c '(foo #(0)) integer?)
                              (foo 3)
                              'pos
                              'neg))
      (set-foo-x! a-foo "not an integer"))) 

  (test/spec-passed
   'prefab/chap-6
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (define a-foo (contract (prefab/c '(foo #(0)) integer?)
                              (foo 3)
                              'pos
                              'neg))
      (set-foo-x! a-foo 15)))

  (test/neg-blame
   'prefab/chap-7
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (define a-foo (contract (prefab/c '(foo #(0)) (-> string? string?))
                              (foo (λ (x) x))
                              'pos
                              'neg))
      (set-foo-x! a-foo (λ (x) 5))
      ((foo-x a-foo) "foo")))

  (test/neg-blame
   'prefab/chap-8
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (define a-foo (contract (prefab/c '(foo #(0)) (-> string? string?))
                              (foo (λ (x) x))
                              'pos
                              'neg))
      (set-foo-x! a-foo (λ (x) x))
      ((foo-x a-foo) 5)))

  (test/pos-blame
   'prefab/chap-9
   '(let ()
      (struct foo ([x #:mutable]) #:prefab)
      (define a-foo (contract (prefab/c '(foo #(0)) (-> string? string?))
                              (foo (λ (x) 5))
                              'pos
                              'neg))
      ((foo-x a-foo) "foo"))))
