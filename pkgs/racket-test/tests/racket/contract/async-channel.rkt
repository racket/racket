#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-full-contract-namespace 'racket/async-channel)])
  (test/pos-blame
   'async-channel/c1
   '(contract (async-channel/c any/c) #f 'pos 'neg))
  
  (test/pos-blame
   'async-channel/c2
   '(let ([ac (make-async-channel)])
      (async-channel-put ac #f)
      (async-channel-get (contract (async-channel/c integer?) ac 'pos 'neg))))
  
  (test/neg-blame
   'async-channel/c3
   '(let ([ac (make-async-channel)])
      (async-channel-put (contract (async-channel/c integer?) ac 'pos 'neg) #f)))
  
  (test/neg-blame
   'async-channel/c-with-cons/c-inside
   '(let ([ac (contract (async-channel/c (cons/c (-> boolean? boolean?) '()))
               (make-async-channel) 'pos 'neg)])
      (async-channel-put ac (list values))
      ((car (async-channel-get ac)) 3)))

  (test/spec-passed
   'async-channel/c-with-higher-order
   '(let ([ac (contract (or/c (async-channel/c integer?) (integer? . -> . integer?))
               (make-async-channel) 'pos 'neg)])
      (async-channel-put ac 1)
      (async-channel-get ac)))

  (test/spec-passed
   'async-channel/c-with-higher-order2
   '(let ([ac (contract (first-or/c (async-channel/c integer?) (integer? . -> . integer?))
               (make-async-channel) 'pos 'neg)])
      (async-channel-put ac 1)
      (async-channel-get ac))))
