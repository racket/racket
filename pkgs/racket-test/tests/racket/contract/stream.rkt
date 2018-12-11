#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-full-contract-namespace 'racket/stream)])
  (test/pos-blame
   'stream/c1
   '(contract (stream/c any/c) #f 'pos 'neg))
  
  (test/pos-blame
   'stream/c2
   '(stream-first (contract (stream/c integer?) (stream #f) 'pos 'neg)))
  
  (test/pos-blame
   'stream/c3
   '(stream-first (contract (stream/c (and/c integer? positive?)) (in-naturals) 'pos 'neg)))
  
  (test/pos-blame
   'stream/c4
   '(contract (stream/c integer?) '(0 1 2 #f) 'pos 'neg))
  
  (test/spec-passed
   'stream/c5
   '(contract (stream/c integer?) (stream #f) 'pos 'neg))
  
  (test/spec-passed
   'stream/c6
   '(stream-first (stream-rest (contract (stream/c (and/c integer? (or/c 0 positive?)))
                                         (in-naturals) 'pos 'neg))))
  
  (test/pos-blame
   'stream/c7
   '(stream-first (stream-rest (contract (stream/c (and/c integer? (or/c 0 positive?)))
                                         (stream 0 -1) 'pos 'neg))))

  (test/spec-passed
   'stream/c8
   '(stream-first (stream-rest (contract (stream/c (and/c integer? (first-or/c 0 positive?)))
                                         (in-naturals) 'pos 'neg))))
  
  (test/pos-blame
   'stream/c9
   '(stream-first (stream-rest (contract (stream/c (and/c integer? (first-or/c 0 positive?)))
                                         (stream 0 -1) 'pos 'neg))))

  (test/spec-passed
   'stream/impersonator-ctc
   '(contract (stream/c (make-contract #:first-order (lambda (x) #t))) empty-stream 'pos 'neg)))
