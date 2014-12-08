#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])

  (test/pos-blame
   'evt/c-first-order-1
   '(contract (evt/c) 5 'pos 'neg))

  (test/spec-passed
   'evt/c-first-order-2
   '(contract (evt/c) always-evt 'pos 'neg))

  (test/pos-blame
   'evt/c-higher-order-1
   '(let ([evt (contract (evt/c symbol?)
                         (handle-evt always-evt (λ (x) 0))
                         'pos 'neg)])
      (sync evt)))

  ;; return arity test
  (test/pos-blame
   'evt/c-higher-order-2
   '(let ([evt (contract (evt/c symbol? number?)
                         (handle-evt always-evt (λ (x) 0))
                         'pos 'neg)])
      (sync evt)))

  (test/spec-passed
   'evt/c-higher-order-3
   '(let ([evt (contract (evt/c symbol? number?)
                         (handle-evt always-evt (λ (x) (values 'a 0)))
                         'pos 'neg)])
      (sync evt)))

  (test/neg-blame
   'evt/c-higher-order-4
   '(let ([f (contract (-> (evt/c symbol?) number?)
                       (λ (e) 0)
                       'pos 'neg)])
      (f 'not-an-evt)))

  (test/pos-blame
   'evt/c-higher-order-5
   '(let ([f (contract (-> (evt/c void?))
                       (λ () 0)
                       'pos 'neg)])
      (f)))

  (test/pos-blame
   'evt/c-higher-order-6
   '(let ([f (contract (-> (evt/c void?))
                       (λ () always-evt)
                       'pos 'neg)])
      (sync (f))))

  (test/spec-passed
   'evt/c-higher-order-7
   '(let ([f (contract (-> (evt/c evt?))
                       (λ () always-evt)
                       'pos 'neg)])
      (sync (f)))))

